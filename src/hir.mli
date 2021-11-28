open Core

module Ty : sig
  module Var : Uid_intf.S

  module Constructor : sig
    type t =
      | Int
      | Bool
      | Arrow
      | Tuple
    [@@deriving sexp, compare, hash, equal]
  end

  type t =
    | Var of Var.t
    | Constructor of Constructor.t * t list
  [@@deriving sexp, compare, hash, equal]

  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val is_poly : t -> bool
  val free_vars : t -> Var.t list
  val to_string : t -> string
  val to_string_hum : t -> string
end

module Var : Named_var_intf.S
module SumCon : Named_var_intf.S

type 'a exp =
  | Var of Var.t
  | Int of int
  | Bool of bool
  | Tuple of 'a tyexp list
  | Split of 'a tyexp * Var.t list * 'a tyexp
  | Inject of SumCon.t * 'a tyexp
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Var.t * 'a tyexp
  | Let of Var.t * 'a tyexp * 'a tyexp

and 'a tyexp = 'a * 'a exp [@@deriving sexp]

val map_exp : f:('a -> 'b) -> 'a tyexp -> 'b tyexp

type 'a stmt = LetStmt of Var.t * 'a tyexp [@@deriving sexp]

val map_stmt : f:('a -> 'b) -> 'a stmt -> 'b stmt

module TySym : Named_var_intf.S

type 'a tydecl = (SumCon.t * 'a option) list [@@deriving sexp]

val map_tydecl : 'a tydecl -> f:('a -> 'b) -> 'b tydecl

type 'a prog =
  { tydecls : 'a tydecl TySym.Map.t
  ; stmts : 'a stmt list
  }
[@@deriving sexp]

type program = Ty.t prog [@@deriving sexp]
