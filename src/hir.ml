open Core

module Ty = struct
  module Var = Uid.Make ()

  module T = struct
    type t =
      | Int
      | Var of Var.t
      | Arrow of t * t
    [@@deriving sexp, compare, hash, equal]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let rec is_poly = function
    | Int -> false
    | Var _ -> true
    | Arrow (t1, t2) -> is_poly t1 || is_poly t2
  ;;

  let rec free_vars = function
    | Int -> []
    | Var v -> [ v ]
    | Arrow (t1, t2) -> free_vars t1 @ free_vars t2
  ;;

  let rec to_string = function
    | Int -> "Int"
    | Var v -> Var.to_string v
    | Arrow (t1, t2) -> [%string "(%{to_string t1} -> %{to_string t2})"]
  ;;
end

module Var = struct
  module Id = Uid.Make ()

  module T = struct
    type t = string * Id.t [@@deriving hash, sexp, compare]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let create n = n, Id.create ()
  let to_string (n, id) = n ^ "#" ^ Id.to_string id
  let name (n, _) = n
end

type 'a exp =
  | Var of Var.t
  | Int of int
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Var.t * 'a tyexp
  | Let of Var.t * 'a tyexp * 'a tyexp

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

let string_of_tyexp_custom ~ty_to_string e =
  let rec go (ty, exp) =
    let ty_str = ty_to_string ty in
    match exp with
    | Var v -> [%string "(%{Var.to_string v} : %{ty_str})"]
    | Int i -> [%string "(%{i#Int} : %{ty_str})"]
    | Ap (e1, e2) -> [%string "(%{go e1} %{go e2} : %{ty_str})"]
    | Abs (x, e) -> [%string "((fun %{Var.to_string x} -> %{go e}) : %{ty_str})"]
    | Let (x, e1, e2) ->
      [%string "((let %{Var.to_string x} = %{go e1} in %{go e2}) : %{ty_str})"]
  in
  go e
;;

let string_of_tyexp = string_of_tyexp_custom ~ty_to_string:Ty.to_string
