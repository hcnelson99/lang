open! Core

(* TODO: need to handle types with free tyvars *)

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
  (* TODO: This is definitely not gonna work inside of a function because the type
   * variables for that function become free. We will have to think about the
   * best way to handle this *)
  assert (not (Hir.Ty.is_poly mono_ty));
  let map = Hir.Ty.Var.Table.create () in
  let rec go poly mono =
    let open Hir.Ty in
    match poly, mono with
    | _, Var _ -> failwith "ICE: rhs is supposed to be monoty"
    | Int, Int -> ()
    | Int, _ -> failwith "ICE: couldn't map type instantiation"
    | Var v, ty ->
      (match Hashtbl.add map ~key:v ~data:ty with
      | `Ok -> ()
      | `Duplicate -> assert (Hir.Ty.equal (Hashtbl.find_exn map v) ty))
    | Arrow (p1, p2), Arrow (m1, m2) ->
      go p1 m1;
      go p2 m2
    | Arrow _, _ -> failwith "ICE: couldn't map type instantiation"
  in
  go poly_ty mono_ty;
  map
;;

let specialize mapping e =
  let rec f = function
    | Hir.Ty.Int -> Hir.Ty.Int
    | Hir.Ty.Var v -> Hashtbl.find_exn mapping v
    | Hir.Ty.Arrow (t1, t2) -> Hir.Ty.Arrow (f t1, f t2)
  in
  Hir.map_ty ~f e
;;

let monomorphize e =
  let table = Hir.Var.Table.create () in
  let rec go ((ty, e) as tyexp) =
    match e with
    | Hir.Var v ->
      (match Hashtbl.find table v with
      | None -> tyexp
      | Some insts ->
        let default () = Hir.Var.create (Hir.Var.name v) in
        ty, Hir.Var (Hashtbl.find_or_add insts ty ~default))
    | Hir.Int _ -> tyexp
    | Hir.Ap (e1, e2) -> ty, Hir.Ap (go e1, go e2)
    | Hir.Abs (v, e) -> ty, Hir.Abs (v, go e)
    | Hir.Let (v, ((e1_ty, _) as e1), e2) ->
      let insts = Hir.Ty.Table.create () in
      Hashtbl.set table ~key:v ~data:insts;
      let ((e2_ty, _) as init) = go e2 in
      Hashtbl.fold insts ~init ~f:(fun ~key:inst_ty ~data:inst_v e ->
          let mapping = map_poly_type_to_mono_type e1_ty inst_ty in
          let e1' = specialize mapping e1 in
          e2_ty, Hir.Let (inst_v, go e1', e))
  in
  go e
;;
