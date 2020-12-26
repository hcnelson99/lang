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

let rec string_of_tyexp (_, exp) = string_of_exp exp

and string_of_exp = function
  | Var v -> [%string "(Var %{Var.to_string v})"]
  | Int i -> [%string "(IntConst %{i#Int})"]
  | Ap (e1, e2) -> [%string "(Ap %{string_of_tyexp e1} %{string_of_tyexp e2})"]
  | Abs (x, e) -> [%string "(Abs %{Var.to_string x} %{string_of_tyexp e})"]
  | Let (x, e1, e2) ->
    [%string "(Let %{Var.to_string x} %{string_of_tyexp e1} %{string_of_tyexp e2})"]
;;
