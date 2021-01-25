open Core

module Ty = struct
  module Var = struct
    include Uid.Make ()

    let to_string t = "t" ^ to_string t
  end

  module Constructor = struct
    type t =
      | Int
      | Bool
      | Arrow
      | Tuple
    [@@deriving sexp, compare, hash, equal]
  end

  module T = struct
    type t =
      | Var of Var.t
      | Constructor of Constructor.t * t list
    [@@deriving sexp, compare, hash, equal]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let rec is_poly = function
    | Var _ -> true
    | Constructor (_, ts) -> List.fold ~init:false ~f:(fun b x -> b || is_poly x) ts
  ;;

  let rec free_vars = function
    | Var v -> [ v ]
    | Constructor (_, ts) -> List.concat_map ~f:free_vars ts
  ;;

  let rec to_string_custom ~var_to_string = function
    | Constructor (Int, []) -> "Int"
    | Constructor (Int, _) -> failwith "invalid int arity"
    | Constructor (Bool, []) -> "Bool"
    | Constructor (Bool, _) -> failwith "invalid bool arity"
    | Var v -> var_to_string v
    | Constructor (Tuple, []) -> "unit"
    | Constructor (Tuple, ts) ->
      List.map ~f:(to_string_paren ~var_to_string) ts |> String.concat ~sep:" * "
    | Constructor (Arrow, [ t1; t2 ]) ->
      [%string
        "%{(to_string_paren ~var_to_string) t1} -> %{(to_string_paren ~var_to_string) t2}"]
    | Constructor (Arrow, _) -> failwith "invalid arrow arity"

  and to_string_paren ~var_to_string t =
    let parenthesize =
      match t with
      | Var _ -> false
      | Constructor (c, ts) ->
        (match c with
        | Int | Bool -> false
        | Arrow -> true
        | Tuple ->
          (match ts with
          | [] -> false
          | _ -> true))
    in
    let res = to_string_custom ~var_to_string t in
    if parenthesize then "(" ^ res ^ ")" else res
  ;;

  let to_string = to_string_custom ~var_to_string:Var.to_string

  let to_string_hum t =
    let var_to_string =
      let names = Var.Table.create () in
      let next_name = ref 'a' in
      let new_name () =
        let res = !next_name in
        next_name := Char.of_int_exn (Char.to_int res + 1);
        "'" ^ Char.to_string res
      in
      Hashtbl.find_or_add names ~default:new_name
    in
    to_string_custom ~var_to_string t
  ;;
end

module Var = struct
  module Id = Uid.Make ()

  module T = struct
    type t = string * Id.t [@@deriving hash, sexp, compare]
  end

  include T
  include Hashable.Make (T)
  include Comparable.Make (T)

  let create n = n, Id.create ()
  let to_string (n, id) = n ^ "#" ^ Id.to_string id
  let name (n, _) = n
end

type 'a exp =
  | Var of Var.t
  | Int of int
  | Bool of bool
  | Tuple of 'a tyexp list
  | Split of 'a tyexp * Var.t list * 'a tyexp
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Var.t * 'a tyexp
  | Let of Var.t * 'a tyexp * 'a tyexp

and 'a tyexp = 'a * 'a exp

type program = Ty.t tyexp

let rec map_ty ~f (ty, exp) =
  ( f ty
  , match exp with
    | Var v -> Var v
    | Int i -> Int i
    | Bool b -> Bool b
    | Tuple ts -> Tuple (List.map ~f:(map_ty ~f) ts)
    | Split (e1, vs, e2) -> Split (map_ty ~f e1, vs, map_ty ~f e2)
    | Ap (e1, e2) -> Ap (map_ty ~f e1, map_ty ~f e2)
    | Abs (v, e) -> Abs (v, map_ty ~f e)
    | Let (v, e1, e2) -> Let (v, map_ty ~f e1, map_ty ~f e2) )
;;

open Caml.Format

let format_tyexp_custom ~ty_to_string f e =
  let rec go f exp =
    match exp with
    | Var v -> fprintf f "%s" (Var.to_string v)
    | Int i -> fprintf f "%d" i
    | Bool b -> fprintf f "%s" (Bool.to_string b)
    | Tuple ts ->
      (match ts with
      | [] -> fprintf f "()"
      | [ _ ] -> failwith "cannot have arity 1 tuple"
      | t :: ts ->
        fprintf f "@[(%a" go_ty t;
        List.iter ts ~f:(fun t -> fprintf f ",@ %a" go_ty t);
        fprintf f "@,)@]")
    | Split ((ty, e1), vs, (_, e2)) ->
      fprintf
        f
        "@[<v>split @[<4>%a@ :@ %s with@ %s@] in@ %a@]"
        go
        e1
        (ty_to_string ty)
        ("(" ^ (vs |> List.map ~f:Var.to_string |> String.concat ~sep:", ") ^ ")")
        go
        e2
    | Ap (e1, e2) -> fprintf f "@[<hov 4>%a@ %a@]" go_ty e1 go_ty e2
    | Abs (x, e) -> fprintf f "@[<4>fun %s ->@ %a@]" (Var.to_string x) go_ty e
    (* We don't show the let's type since we assume it's correct *)
    | Let (x, (ty, e1), (_, e2)) ->
      fprintf
        f
        "@[<v>let @[<4>%s@ :@ %s =@ %a@] in@ %a@]"
        (Var.to_string x)
        (ty_to_string ty)
        go
        e1
        go
        e2
  and go_ty f (ty, exp) =
    match exp with
    | Int _ | Bool _ -> go f exp
    | Var _ | Ap _ | Abs _ | Let _ | Tuple _ | Split _ ->
      fprintf f "@[<hv>(%a@ : %s)@]" go exp (ty_to_string ty)
  in
  go_ty f e
;;

let format = format_tyexp_custom ~ty_to_string:Ty.to_string
