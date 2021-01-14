module Ty = struct
  module Var = Var.MkVar ()

  type t =
    | Int
    | Var of Var.t
    | Arrow of t * t

  let rec to_string = function
    | Int -> "Int"
    | Var v -> Var.to_string v
    | Arrow (t1, t2) -> [%string "(%{to_string t1} -> %{to_string t2})"]
  ;;
end

type 'a exp =
  | Var of Symbol.t
  | Int of int
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Symbol.t * 'a tyexp
  | Let of Symbol.t * 'a tyexp * 'a tyexp

and 'a tyexp = 'a * 'a exp

type program = Ty.t tyexp

let rec map_ty ~f (ty, exp) =
  ( f ty
  , match exp with
    | Var v -> Var v
    | Int i -> Int i
    | Ap (e1, e2) -> Ap (map_ty ~f e1, map_ty ~f e2)
    | Abs (v, e) -> Abs (v, map_ty ~f e)
    | Let (v, e1, e2) -> Let (v, map_ty ~f e1, map_ty ~f e2) )
;;

let rec string_of_tyexp (ty, exp) =
  let ty_str = Ty.to_string ty in
  match exp with
  | Var v -> [%string "(%{Symbol.to_string v} : %{ty_str})"]
  | Int i -> [%string "(%{i#Int} : %{ty_str})"]
  | Ap (e1, e2) -> [%string "(%{string_of_tyexp e1} %{string_of_tyexp e2} : %{ty_str})"]
  | Abs (x, e) ->
    [%string "((fun %{Symbol.to_string x} -> %{string_of_tyexp e}) : %{ty_str})"]
  | Let (x, e1, e2) ->
    [%string
      "((let %{Symbol.to_string x} = %{string_of_tyexp e1} in %{string_of_tyexp e2}) : \
       %{ty_str})"]
;;
