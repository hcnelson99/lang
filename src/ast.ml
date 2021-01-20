open! Core

type msym = Symbol.t Mark.t
type binop = Plus

(* let string_of_binop = function *)
(*   | Plus -> "+" *)
(* ;; *)

type mexp = exp Mark.t

and exp =
  | Var of Symbol.t
  | Int of int
  (* | Binop of binop * mexp * mexp *)
  | Ap of mexp * mexp
  | Abs of msym * mexp
  | Let of msym * mexp * mexp

type program = mexp

let rec string_of_mexp mexp =
  match Mark.obj mexp with
  | Var v -> [%string "(Var %{Symbol.name v})"]
  | Int i -> [%string "(IntConst %{i#Int})"]
  (* | Binop (op, e1, e2) -> *)
  (*   [%string "(Binop %{string_of_binop op} %{string_of_mexp e1} %{string_of_mexp e2})"] *)
  | Ap (e1, e2) -> [%string "(Ap %{string_of_mexp e1} %{string_of_mexp e2})"]
  | Abs (x, e) -> [%string "(Abs %{Symbol.name (Mark.obj x)} %{string_of_mexp e})"]
  | Let (x, e1, e2) ->
    [%string
      "(Let %{Symbol.name (Mark.obj x)} %{string_of_mexp e1} %{string_of_mexp e2})"]
;;

let string_of_program = string_of_mexp
