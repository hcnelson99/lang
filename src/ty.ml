open Core

(* TODO: refactor to be internal to typechecker? *)

include Uid.Make ()

module Constructor = struct
  type t =
    | Int
    | Bool
    | Arrow
  [@@deriving equal]
end

type data =
  | Self
  | Link of t
  | Constructor of Constructor.t * t list

let union_find_table : data Table.t = Table.create ()

let unconstrained () =
  let ty = create () in
  Hashtbl.add_exn union_find_table ~key:ty ~data:Self;
  ty
;;

let constructor c ts =
  let ty = create () in
  Hashtbl.add_exn union_find_table ~key:ty ~data:(Constructor (c, ts));
  ty
;;

(* TODO: do path compression *)
(* Right now find is expensive and recursive. We should do the ocaml in-place
 * thing so that unification updates all the types in place (aka the path
 * compression) *)
module Find = struct
  type nonrec t =
    | Var of t
    | Constructor of Constructor.t * t list

  let rec find t =
    match Hashtbl.find_exn union_find_table t with
    | Self -> Var t
    | Constructor (c, ts) -> Constructor (c, ts)
    | Link t -> find t
  ;;
end

(* let to_string_custom ~var_to_string t = *)
(*   let rec go t = *)
(*     match Find.find t with *)
(*     | Var v -> var_to_string v *)
(*     | Constructor (Arrow, [ t1; t2 ]) -> [%string "%{go_paren t1} -> %{go_paren t2}"] *)
(*     | Constructor (Arrow, _) -> failwith "invalid arrow arity" *)
(*     | Constructor (Int, []) -> "Int" *)
(*     | Constructor (Int, _) -> failwith "invalid int arity" *)
(*     | Constructor (Bool, []) -> "Bool" *)
(*     | Constructor (Bool, _) -> failwith "invalid bool arity" *)
(*   and go_paren t = "(" ^ go t ^ ")" in *)
(*   go t *)
(* ;; *)

(* let to_string_debug = to_string *)

(* let to_string t = *)
(*   let var_to_string = *)
(*     let names = Table.create () in *)
(*     let next_name = ref 'a' in *)
(*     let new_name () = *)
(*       let res = !next_name in *)
(*       next_name := Char.of_int_exn (Char.to_int res + 1); *)
(*       "'" ^ Char.to_string res *)
(*     in *)
(*     Hashtbl.find_or_add names ~default:new_name *)
(*   in *)
(*   to_string_custom ~var_to_string t *)
(* ;; *)

let rec is_poly t =
  match Find.find t with
  | Find.Var _ -> true
  | Find.Constructor (_, ts) ->
    List.fold ts ~init:false ~f:(fun acc t -> acc || is_poly t)
;;

module Union_find = struct
  exception Unification_error

  (* TODO: check for circularity *)
  let rec unify t0 t1 =
    match Find.find t0, Find.find t1 with
    | Find.Constructor (c, ts), Find.Constructor (c', ts') ->
      if Constructor.equal c c'
      then (
        match List.zip ts ts' with
        | List.Or_unequal_lengths.Unequal_lengths ->
          failwith "ICE: equal constructors with different arities?"
        | List.Or_unequal_lengths.Ok z -> List.iter z ~f:(fun (x, y) -> unify x y))
      else raise Unification_error
    | Find.Var v1, Find.Var v2 -> Hashtbl.set union_find_table ~key:v1 ~data:(Link v2)
    | Find.Var v, _ -> Hashtbl.set union_find_table ~key:v ~data:(Link t1)
    | _, Find.Var v -> Hashtbl.set union_find_table ~key:v ~data:(Link t0)
  ;;

  let instantiate t =
    let find_or_create =
      let table = Table.create () in
      Hashtbl.find_or_add table ~default:unconstrained
    in
    let rec go t =
      match Find.find t with
      | Find.Var t -> find_or_create t
      | Find.Constructor (c, ts) -> constructor c (List.map ~f:go ts)
    in
    go t
  ;;
end

let to_hir_table = Table.create ()

let rec to_hir_ty t =
  match Find.find t with
  | Find.Var v ->
    Hir.Ty.Var (Hashtbl.find_or_add to_hir_table v ~default:Hir.Ty.Var.create)
  | Find.Constructor (Constructor.Int, []) -> Hir.Ty.Int
  | Find.Constructor (Constructor.Int, _) -> failwith "invalid int arity"
  | Find.Constructor (Constructor.Bool, []) -> Hir.Ty.Bool
  | Find.Constructor (Constructor.Bool, _) -> failwith "invalid bool arity"
  | Find.Constructor (Constructor.Arrow, [ t1; t2 ]) ->
    Hir.Ty.Arrow (to_hir_ty t1, to_hir_ty t2)
  | Find.Constructor (Constructor.Arrow, _) -> failwith "invalid arrow arity"
;;
