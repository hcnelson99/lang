type exp =
  | Var of Symbol.t
  | Int of int
  | Ap of tyexp * tyexp
  | Abs of Symbol.t * tyexp
  | Let of Symbol.t * tyexp * tyexp

and tyexp = Ty.t * exp

type program = tyexp

let rec string_of_tyexp (ty, exp) =
  let ty_str = Ty.to_string ty in
  match exp with
  | Var v -> [%string "(%{Symbol.to_string v} : %{ty_str})"]
  | Int i -> [%string "(%{i#Int} : %{ty_str})"]
  | Ap (e1, e2) -> [%string "(%{string_of_tyexp e1} %{string_of_tyexp e2} : %{ty_str})"]
  | Abs (x, e) ->
    [%string "((fun %{Symbol.to_string x} -> %{string_of_tyexp e}) : %{ty_str})"]
  | Let (x, e1, e2) ->
    [%string
      "((let %{Symbol.to_string x} = %{string_of_tyexp e1} in %{string_of_tyexp e2}) : \
       %{ty_str})"]
;;
