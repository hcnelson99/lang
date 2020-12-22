open! Core

type msym = Symbol.t Mark.t

type mexp = exp Mark.t
and exp =
    | Var of Symbol.t
    | Ap of mexp * mexp
    | Abs of msym * mexp
    | Let of msym * mexp * mexp

type program = mexp

val string_of_program : program -> string
