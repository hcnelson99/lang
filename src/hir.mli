type ty =
  | Int
  | Arrow of ty * ty

type exp =
  | Var of Var.t
  | Int of int
  | Ap of tyexp * tyexp
  | Abs of Var.t * tyexp
  | Let of Var.t * tyexp * tyexp

and tyexp = ty * exp

type program = tyexp

val string_of_tyexp : tyexp -> string
val string_of_exp : exp -> string
