type ty = Foo

type exp =
  | Var of Var.t
  | Int of int
  | Ap of exp * exp
  | Abs of Var.t * exp
  | Let of Var.t * exp * exp

type program = exp

let rec string_of_exp = function
  | Var v -> [%string "(Var %{Var.to_string v})"]
  | Int i -> [%string "(IntConst %{i#Int})"]
  (* | Binop (op, e1, e2) -> *)
  (*   [%string "(Binop %{string_of_binop op} %{string_of_mexp e1} %{string_of_mexp e2})"] *)
  | Ap (e1, e2) -> [%string "(Ap %{string_of_exp e1} %{string_of_exp e2})"]
  | Abs (x, e) -> [%string "(Abs %{Var.to_string x} %{string_of_exp e})"]
  | Let (x, e1, e2) ->
    [%string "(Let %{Var.to_string x} %{string_of_exp e1} %{string_of_exp e2})"]
;;
