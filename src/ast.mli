open! Core

type msym = Symbol.t Mark.t
type binop = Plus

type mexp = exp Mark.t

and exp =
  | Var of Symbol.t
  | Int of int
  | Bool of bool
  (* | Binop of binop * mexp * mexp *)
  | Ap of mexp * mexp
  | Abs of msym * mexp
  | Let of msym * mexp * mexp

type program = mexp

val format : Format.formatter -> program -> unit
