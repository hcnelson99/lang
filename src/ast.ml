open! Core
include Ast_intf.S
open Caml.Format

let str f s = fprintf f "%s" s

let rec format_mexp f mexp =
  match Mark.obj mexp with
  | Var v -> str f [%string "(Var %{Symbol.name v})"]
  | Bool b -> str f [%string "(Var %{Bool.to_string b})"]
  | Int i -> str f [%string "(IntConst %{i#Int})"]
  | Tuple ts ->
    (match ts with
    | [] -> str f "()"
    | [ _ ] -> failwith "cannot have arity 1 tuple"
    | t :: ts ->
      fprintf f "@[(%a" format_mexp t;
      List.iter ts ~f:(fun t -> fprintf f "%a,@ " format_mexp t);
      fprintf f "@,)@]")
  (* | Binop (op, e1, e2) -> *)
  (*   [%string "(Binop %{string_of_binop op} %{string_of_mexp e1} %{string_of_mexp e2})"] *)
  | Ap (e1, e2) -> fprintf f "@[<2>(Ap@ @[<hv 2>%a@ %a@])@]" format_mexp e1 format_mexp e2
  | Abs (x, e) ->
    fprintf f "@[<2>(Abs@ @[<hv 2>%s@ %a@])@]" (Symbol.name (Mark.obj x)) format_mexp e
  | Let (x, e1, e2) ->
    fprintf
      f
      "@[<v>(Let %s %a@,%a)@]"
      (Symbol.name (Mark.obj x))
      format_mexp
      e1
      format_mexp
      e2
  | Split (e1, is, e2) ->
    let vars =
      is |> List.map ~f:(fun x -> Symbol.name (Mark.obj x)) |> String.concat ~sep:", "
    in
    fprintf f "@[<v>(Split %a (%s)@,%a)@]" format_mexp e1 vars format_mexp e2
;;

let format = format_mexp
