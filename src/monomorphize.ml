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

type mapping = Hir.Ty.t Hir.Ty.Var.Table.t

type ty_insts =
  | Mono
  | Poly of
      { poly_ty : Hir.Ty.t
      ; insts : mapping Hir.Ty.Map.t
      }

(* TODO: come up with a better name than "context" *)
(* TODO: no real reason for this to be mutable table instead of a map other than that i'm generally lazy about threading things around *)
type poly_insts = ty_insts Symbol.Table.t

let print_poly_insts (poly_insts : poly_insts) =
  Hashtbl.iteri poly_insts ~f:(fun ~key ~data ->
      print_string (Symbol.to_string key);
      print_string " -> ";
      match data with
      | Mono -> print_endline "mono"
      | Poly { poly_ty; insts } ->
        print_endline (Hir.Ty.to_string poly_ty);
        Map.iteri insts ~f:(fun ~key:mono_ty ~data:mapping ->
            print_endline [%string "\tinst for type %{Hir.Ty.to_string mono_ty}"];
            Hashtbl.iteri mapping ~f:(fun ~key ~data ->
                print_endline
                  [%string "\t\t%{Hir.Ty.Var.to_string key} -> %{Hir.Ty.to_string data}"])))
;;

let defined_as_mono poly_insts v = Hashtbl.add_exn poly_insts ~key:v ~data:Mono

let defined_as_poly poly_insts v ty =
  if Hir.Ty.is_poly ty
  then
    Hashtbl.add_exn
      poly_insts
      ~key:v
      ~data:(Poly { poly_ty = ty; insts = Hir.Ty.Map.empty })
  else defined_as_mono poly_insts v
;;

let used_as poly_insts v ty =
  match Hashtbl.find poly_insts v with
  | None -> failwith "ICE: var never defined"
  | Some Mono -> ()
  | Some (Poly { poly_ty; insts }) ->
    let inst = map_poly_type_to_mono_type poly_ty ty in
    (* TODO: n^2 very slow *)
    let insts =
      match Map.add insts ~key:ty ~data:inst with
      | `Ok res -> res
      | `Duplicate -> insts
    in
    Hashtbl.set poly_insts ~key:v ~data:(Poly { poly_ty; insts })
;;

let analyze_polymorphic_uses e =
  let rec go (poly_insts : poly_insts) (ty, exp) =
    match exp with
    | Hir.Var v -> used_as poly_insts v ty
    | Hir.Int _ -> ()
    | Hir.Ap (e1, e2) ->
      go poly_insts e1;
      go poly_insts e2
    | Hir.Abs (v, e) ->
      defined_as_mono poly_insts v;
      go poly_insts e
    | Hir.Let (v, e1, e2) ->
      go poly_insts e1;
      let t, _ = e1 in
      defined_as_poly poly_insts v t;
      go poly_insts e2
  in
  let poly_insts = Symbol.Table.create () in
  go poly_insts e;
  poly_insts
;;

let specialize mapping e =
  let rec f = function
    | Hir.Ty.Int -> Hir.Ty.Int
    | Hir.Ty.Var v -> Hashtbl.find_exn mapping v
    | Hir.Ty.Arrow (t1, t2) -> Hir.Ty.Arrow (f t1, f t2)
  in
  Hir.map_ty ~f e
;;

let clone_lets poly_insts e =
  let module T = struct
    type t = Symbol.t * Hir.Ty.t [@@deriving hash, compare, sexp]
  end
  in
  let module Var_inst_table = Hashtbl.Make (T) in
  let var_inst_table = Var_inst_table.create () in
  let counter =
    let x = ref 0 in
    fun () ->
      let r = !x in
      x := r + 1;
      Int.to_string r
  in
  let rec go ((ty, e) as tyexp) =
    match e with
    | Hir.Var v ->
      let () = print_endline ("var " ^ Symbol.to_string v) in
      let () = print_endline (Hir.Ty.to_string ty) in
      (match Hashtbl.find var_inst_table (v, ty) with
      | None -> tyexp
      | Some v' -> ty, Hir.Var v')
    | Hir.Int _ -> tyexp
    | Hir.Ap (e1, e2) -> ty, Hir.Ap (go e1, go e2)
    | Hir.Abs (v, e) -> ty, Hir.Abs (v, go e)
    | Hir.Let (v, e1, e2) ->
      (match Hashtbl.find_exn poly_insts v with
      | Mono -> ty, Hir.Let (v, go e1, go e2)
      | Poly { poly_ty = _; insts } ->
        let e1' = go e1 in
        let k =
          Map.fold
            insts
            ~init:(fun x -> x)
            ~f:(fun ~key:mono_ty ~data:mapping k ->
              let e1'' = specialize mapping e1' in
              let v' =
                Symbol.of_string (Symbol.to_string v ^ "_polymorph_" ^ counter ())
              in
              let () = print_endline (Symbol.to_string v) in
              let () = print_endline (Symbol.to_string v') in
              let () = print_endline (Hir.Ty.to_string mono_ty) in
              Hashtbl.add_exn var_inst_table ~key:(v, mono_ty) ~data:v';
              fun x -> ty, Hir.Let (v', e1'', k x))
        in
        k (go e2))
  in
  go e
;;

let monomorphize e =
  let polymorphic_uses = analyze_polymorphic_uses e in
  print_poly_insts polymorphic_uses;
  clone_lets polymorphic_uses e
;;
