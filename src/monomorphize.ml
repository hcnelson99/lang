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

type ty_insts =
  | Mono
  | Poly of
      { poly_ty : Hir.Ty.t
      ; insts : Hir.Ty.t Hir.Ty.Var.Table.t list
      }

(* TODO: no real reason for this to be mutable table instead of a map other than that i'm generally lazy about threading things around *)

type context = ty_insts Symbol.Table.t

let print_context (ctx : context) =
  Hashtbl.iteri ctx ~f:(fun ~key ~data ->
      print_string (Symbol.to_string key);
      print_string " -> ";
      match data with
      | Mono -> print_endline "mono"
      | Poly { poly_ty; insts } ->
        print_endline (Hir.Ty.to_string poly_ty);
        List.iter insts ~f:(fun inst ->
            print_endline "\tinst:";
            Hashtbl.iteri inst ~f:(fun ~key ~data ->
                print_endline
                  [%string "\t\t%{Hir.Ty.Var.to_string key} -> %{Hir.Ty.to_string data}"])))
;;

let defined_as_mono ctx v = Hashtbl.add_exn ctx ~key:v ~data:Mono

let defined_as_poly ctx v ty =
  if Hir.Ty.is_poly ty
  then Hashtbl.add_exn ctx ~key:v ~data:(Poly { poly_ty = ty; insts = [] })
  else defined_as_mono ctx v
;;

let used_as ctx v ty =
  match Hashtbl.find ctx v with
  | None -> failwith "ICE: var never defined"
  | Some Mono -> ()
  | Some (Poly { poly_ty; insts }) ->
    let inst = map_poly_type_to_mono_type poly_ty ty in
    (* TODO: n^2 very slow *)
    if List.exists insts ~f:(fun x -> Hir.Ty.Var.Table.equal Hir.Ty.equal inst x)
    then ()
    else Hashtbl.set ctx ~key:v ~data:(Poly { poly_ty; insts = inst :: insts })
;;

let rec record_polymorphic_uses (ctx : context) (ty, exp) =
  match exp with
  | Hir.Var v -> used_as ctx v ty
  | Hir.Int _ -> ()
  | Hir.Ap (e1, e2) ->
    record_polymorphic_uses ctx e1;
    record_polymorphic_uses ctx e2
  | Hir.Abs (v, e) ->
    defined_as_mono ctx v;
    record_polymorphic_uses ctx e
  | Hir.Let (v, e1, e2) ->
    record_polymorphic_uses ctx e1;
    let t, _ = e1 in
    defined_as_poly ctx v t;
    record_polymorphic_uses ctx e2
;;

let monomorphize e =
  let ctx = Symbol.Table.create () in
  record_polymorphic_uses ctx e;
  print_context ctx;
  failwith ""
;;
