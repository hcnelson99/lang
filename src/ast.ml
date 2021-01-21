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

open Caml.Format

let str f s = fprintf f "%s" s

let rec format_mexp f mexp =
  match Mark.obj mexp with
  | Var v -> str f [%string "(Var %{Symbol.name v})"]
  | Int i -> str f [%string "(IntConst %{i#Int})"]
  (* | Binop (op, e1, e2) -> *)
  (*   [%string "(Binop %{string_of_binop op} %{string_of_mexp e1} %{string_of_mexp e2})"] *)
  | Ap (e1, e2) -> fprintf f "@[(Ap@ @[<1>%a@ %a@]@])" format_mexp e1 format_mexp e2
  | Abs (x, e) ->
    fprintf f "(@[<1>Abs@ @[<1>%s@ %a@]@])" (Symbol.name (Mark.obj x)) format_mexp e
  | Let (x, e1, e2) ->
    fprintf
      f
      "(Let @[<v>%s %a@,%a@])"
      (Symbol.name (Mark.obj x))
      format_mexp
      e1
      format_mexp
      e2
;;

let format f e = fprintf f "%a@." format_mexp e
