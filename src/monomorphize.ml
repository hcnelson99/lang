open! Core

(* Right now we probably polymorph functions like "fun x -> 1" unnecessarily
 * when the 'a in 'a -> Int should just be replaced with unit *)

module Hir_ty_list = struct
  module T = struct
    type t = Hir.Ty.t list [@@deriving sexp, compare, hash]
  end

  include T
  include Comparable.Make (T)
end

let map_poly_type_to_mono_type poly_ty mono_ty =
  assert (Hir.Ty.is_poly poly_ty);
  assert (not (Hir.Ty.is_poly mono_ty));
  let map = Hir.Ty.Var.Table.create () in
  let rec go poly mono =
    let open Hir.Ty in
    match poly, mono with
    | _, Var _ -> failwith "ICE: rhs is supposed to be monoty"
    | Var v, ty ->
      (match Hashtbl.add map ~key:v ~data:ty with
      | `Ok -> ()
      | `Duplicate -> assert (Hir.Ty.equal (Hashtbl.find_exn map v) ty))
    | Constructor (c, ts), Constructor (c', ts') ->
      if Hir.Ty.Constructor.equal c c'
      then (
        match List.zip ts ts' with
        | List.Or_unequal_lengths.Unequal_lengths ->
          failwith "ICE: couldn't unify in monomorphize"
        | List.Or_unequal_lengths.Ok z -> List.iter ~f:(fun (t1, t2) -> go t1 t2) z)
  in
  go poly_ty mono_ty;
  map
;;

let specialize mapping e =
  let rec f t =
    match t with
    | Hir.Ty.Var v ->
      (match Hashtbl.find mapping v with
      | None -> Hir.Ty.Var v
      | Some x -> x)
    | Hir.Ty.Constructor (c, ts) -> Hir.Ty.Constructor (c, List.map ~f ts)
  in
  Hir.map_exp ~f e
;;

let monomorphize { Hir.tydecls; stmts } =
  (* Unnecessary mutability? Probably. *)
  let table = Hir.Var.Table.create () in
  let new_instantiable_var v =
    let insts = Hir.Ty.Table.create () in
    Hashtbl.set table ~key:v ~data:insts;
    insts
  in
  let rec specializations insts ((e_ty, _) as e) =
    insts
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (inst_ty, inst_v) ->
           let mapping = map_poly_type_to_mono_type e_ty inst_ty in
           let e' = specialize mapping e in
           inst_v, monomorphize_exp e')
  and monomorphize_exp ((ty, e) as tyexp) =
    match e with
    | Hir.Int _ | Hir.Bool _ -> tyexp
    | Hir.Var v ->
      (match Hashtbl.find table v with
      | None -> tyexp
      | Some insts ->
        let default () = Hir.Var.create (Hir.Var.name v) in
        ty, Hir.Var (Hashtbl.find_or_add insts ty ~default))
    | Hir.Ap (e1, e2) -> ty, Hir.Ap (monomorphize_exp e1, monomorphize_exp e2)
    | Hir.Tuple es -> ty, Hir.Tuple (List.map ~f:monomorphize_exp es)
    | Hir.Split (e1, vs, e2) ->
      ty, Hir.Split (monomorphize_exp e1, vs, monomorphize_exp e2)
    | Hir.Abs (v, e) -> ty, Hir.Abs (v, monomorphize_exp e)
    | Hir.Let (v, e1, e2) ->
      let insts = new_instantiable_var v in
      let ((e2_ty, _) as init) = monomorphize_exp e2 in
      specializations insts e1
      |> List.fold ~init ~f:(fun e (inst_v, inst_e) -> e2_ty, Hir.Let (inst_v, inst_e, e))
  in
  let rec monomorphize_stmts = function
    | [] -> []
    | s :: ss ->
      (match s with
      | Hir.LetStmt (v, e) ->
        let insts = new_instantiable_var v in
        let ss' = monomorphize_stmts ss in
        let cloned_lets =
          specializations insts e
          |> List.map ~f:(fun (inst_v, inst_e) -> Hir.LetStmt (inst_v, inst_e))
        in
        cloned_lets @ ss')
  in
  { Hir.tydecls; stmts = monomorphize_stmts stmts }
;;
