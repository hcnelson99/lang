open Core

(* TODO: refactor to be internal to typechecker? *)

include Uid.Make ()

(* TODO: unify int, bool, arrow *)
type data =
  | Self
  | Int
  | Bool
  | Link of t
  | Arrow of t * t

let union_find_table : data Table.t = Table.create ()

let data_to_string = function
  | Self -> "self"
  | Int -> "int"
  | Bool -> "bool"
  | Link t -> [%string "Link %{to_string t}"]
  | Arrow (t1, t2) -> [%string "Arrow (%{to_string t1}, %{to_string t2})"]
;;

let debug_print_union_find_table () =
  union_find_table
  |> Hashtbl.to_alist
  |> List.iter ~f:(fun (var, data) ->
         print_endline [%string "%{to_string var} -> %{data_to_string data}"])
;;

let unconstrained () =
  let ty = create () in
  Hashtbl.add_exn union_find_table ~key:ty ~data:Self;
  ty
;;

let arrow (t1, t2) =
  let ty = create () in
  Hashtbl.add_exn union_find_table ~key:ty ~data:(Arrow (t1, t2));
  ty
;;

let int_ =
  let ty = create () in
  Hashtbl.add_exn union_find_table ~key:ty ~data:Int;
  ty
;;

let bool_ =
  let ty = create () in
  Hashtbl.add_exn union_find_table ~key:ty ~data:Bool;
  ty
;;

(* TODO: do path compression *)
(* Right now find is expensive and recursive. We should do the ocaml in-place
 * thing so that unification updates all the types in place (aka the path
 * compression) *)
module Find = struct
  type nonrec t =
    | Var of t
    | Arrow of t * t
    | Int
    | Bool

  let rec find t =
    match Hashtbl.find_exn union_find_table t with
    | Self -> Var t
    | Int -> Int
    | Bool -> Bool
    | Arrow (t1, t2) -> Arrow (t1, t2)
    | Link t -> find t
  ;;
end

let to_string_custom ~var_to_string t =
  let rec go t =
    match Find.find t with
    | Var v -> var_to_string v
    | Arrow (t1, t2) -> [%string "%{go_paren t1} -> %{go_paren t2}"]
    | Int -> "Int"
    | Bool -> "Bool"
  and go_paren t = "(" ^ go t ^ ")" in
  go t
;;

let to_string_debug = to_string

let to_string t =
  let var_to_string =
    let names = Table.create () in
    let next_name = ref 'a' in
    let new_name () =
      let res = !next_name in
      next_name := Char.of_int_exn (Char.to_int res + 1);
      "'" ^ Char.to_string res
    in
    Hashtbl.find_or_add names ~default:new_name
  in
  to_string_custom ~var_to_string t
;;

let rec is_poly t =
  match Find.find t with
  | Find.Var _ -> true
  | Find.Int -> false
  | Find.Bool -> false
  | Find.Arrow (t1, t2) -> is_poly t1 || is_poly t2
;;

module Union_find = struct
  exception Unification_error

  (* TODO: check for circularity *)
  let rec unify t0 t1 =
    match Find.find t0, Find.find t1 with
    | Find.Int, Find.Int -> ()
    | Find.Arrow (p1, p2), Find.Arrow (q1, q2) ->
      unify p1 q1;
      unify p2 q2
    | Find.Var v1, Find.Var v2 -> Hashtbl.set union_find_table ~key:v1 ~data:(Link v2)
    | Find.Var v, _ -> Hashtbl.set union_find_table ~key:v ~data:(Link t1)
    | _, Find.Var v -> Hashtbl.set union_find_table ~key:v ~data:(Link t0)
    | _, _ -> raise Unification_error
  ;;

  let instantiate t =
    let find_or_create =
      let table = Table.create () in
      Hashtbl.find_or_add table ~default:unconstrained
    in
    let rec go t =
      match Find.find t with
      | Find.Var t -> find_or_create t
      | Find.Int -> int_
      | Find.Bool -> bool_
      | Find.Arrow (t1, t2) -> arrow (go t1, go t2)
    in
    go t
  ;;
end

let to_hir_table = Table.create ()

let rec to_hir_ty t =
  match Find.find t with
  | Find.Var v ->
    Hir.Ty.Var (Hashtbl.find_or_add to_hir_table v ~default:Hir.Ty.Var.create)
  | Find.Int -> Hir.Ty.Int
  | Find.Bool -> Hir.Ty.Bool
  | Find.Arrow (t1, t2) -> Hir.Ty.Arrow (to_hir_ty t1, to_hir_ty t2)
;;
