(* TODO: Symbol.t becomes Var.t when we handle alpha-equivalence *)
type exp =
  | Var of Symbol.t
  | Int of int
  | Ap of tyexp * tyexp
  | Abs of Symbol.t * tyexp
  | Let of Symbol.t * tyexp * tyexp

and tyexp = Ty.t * exp

type program = tyexp

val string_of_tyexp : tyexp -> string
