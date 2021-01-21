open Core

module Ty = struct
  module Var = struct
    include Uid.Make ()

    let to_string t = "t" ^ to_string t
  end

  module T = struct
    type t =
      | Int
      | Bool
      | Var of Var.t
      | Arrow of t * t
    [@@deriving sexp, compare, hash, equal]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let rec is_poly = function
    | Int | Bool -> false
    | Var _ -> true
    | Arrow (t1, t2) -> is_poly t1 || is_poly t2
  ;;

  let rec free_vars = function
    | Int | Bool -> []
    | Var v -> [ v ]
    | Arrow (t1, t2) -> free_vars t1 @ free_vars t2
  ;;

  let rec to_string = function
    | Int -> "Int"
    | Bool -> "Bool"
    | Var v -> Var.to_string v
    | Arrow (t1, t2) -> [%string "%{to_string_paren t1} -> %{to_string_paren t2}"]

  and to_string_paren t =
    let parenthesize =
      match t with
      | Int | Var _ | Bool -> false
      | Arrow _ -> true
    in
    let res = to_string t in
    if parenthesize then "(" ^ res ^ ")" else res
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
  | Bool of bool
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
    | Bool b -> Bool b
    | Ap (e1, e2) -> Ap (map_ty ~f e1, map_ty ~f e2)
    | Abs (v, e) -> Abs (v, map_ty ~f e)
    | Let (v, e1, e2) -> Let (v, map_ty ~f e1, map_ty ~f e2) )
;;

open Caml.Format

let format_tyexp_custom ~ty_to_string f e =
  let rec go f exp =
    match exp with
    | Var v -> fprintf f "%s" (Var.to_string v)
    | Int i -> fprintf f "%d" i
    | Bool b -> fprintf f "%s" (Bool.to_string b)
    | Ap (e1, e2) -> fprintf f "@[<hov 4>%a@ %a@]" go_ty e1 go_ty e2
    | Abs (x, e) -> fprintf f "@[<4>fun %s ->@ %a@]" (Var.to_string x) go_ty e
    (* We don't show the let's type since we assume it's correct *)
    | Let (x, (ty, e1), (_, e2)) ->
      fprintf
        f
        "@[<v>let %s : %s = %a in@ %a@]"
        (Var.to_string x)
        (ty_to_string ty)
        go
        e1
        go
        e2
  and go_ty f (ty, exp) =
    match exp with
    | Int _ | Bool _ -> go f exp
    | Var _ | Ap _ | Abs _ | Let _ ->
      fprintf f "@[<hv>(%a@ : %s)@]" go exp (ty_to_string ty)
  in
  go_ty f e
;;

let format = format_tyexp_custom ~ty_to_string:Ty.to_string
