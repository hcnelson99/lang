type exp =
  | Var of Symbol.t
  | Int of int
  | Ap of tyexp * tyexp
  | Abs of Symbol.t * tyexp
  | Let of Symbol.t * tyexp * tyexp

and tyexp = Ast_type.Mono_ty.t * exp

type program = tyexp

let rec string_of_tyexp (ty, exp) =
  let ty_str = Ast_type.Mono_ty.to_string ty in
  match exp with
  | Var v -> [%string "(Var %{Symbol.to_string v} : %{ty_str})"]
  | Int i -> [%string "(IntConst %{i#Int} : %{ty_str})"]
  | Ap (e1, e2) ->
    [%string "(Ap %{string_of_tyexp e1} %{string_of_tyexp e2} : %{ty_str})"]
  | Abs (x, e) -> [%string "(Abs %{Symbol.to_string x} %{string_of_tyexp e} : %{ty_str})"]
  | Let (x, e1, e2) ->
    [%string
      "(Let %{Symbol.to_string x} %{string_of_tyexp e1} %{string_of_tyexp e2} : \
       %{ty_str})"]
;;
