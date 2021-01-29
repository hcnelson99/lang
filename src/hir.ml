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
  let to_string_hum (n, _) = n
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

and 'a tyexp = 'a * 'a exp [@@deriving sexp]

type 'a stmt = LetStmt of Var.t * 'a tyexp [@@deriving sexp]
type program = Ty.t stmt list [@@deriving sexp]

let rec map_exp ~f (ty, exp) =
  ( f ty
  , match exp with
    | Var v -> Var v
    | Int i -> Int i
    | Bool b -> Bool b
    | Tuple ts -> Tuple (List.map ~f:(map_exp ~f) ts)
    | Split (e1, vs, e2) -> Split (map_exp ~f e1, vs, map_exp ~f e2)
    | Ap (e1, e2) -> Ap (map_exp ~f e1, map_exp ~f e2)
    | Abs (v, e) -> Abs (v, map_exp ~f e)
    | Let (v, e1, e2) -> Let (v, map_exp ~f e1, map_exp ~f e2) )
;;

let map_stmt ~f = function
  | LetStmt (v, e) -> LetStmt (v, map_exp ~f e)
;;
