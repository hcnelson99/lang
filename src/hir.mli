open Core

module Ty : sig
  module Var : Var_intf.S

  type t =
    | Int
    | Var of Var.t
    | Arrow of t * t
  [@@deriving sexp, compare, hash, equal]

  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val is_poly : t -> bool
  val free_vars : t -> Var.t list
  val to_string : t -> string
end

(* TODO: Symbol.t becomes Var.t when we handle alpha-equivalence *)

type 'a exp =
  | Var of Symbol.t
  | Int of int
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Symbol.t * 'a tyexp
  | Let of Symbol.t * 'a tyexp * 'a tyexp

and 'a tyexp = 'a * 'a exp

val map_ty : f:('a -> 'b) -> 'a tyexp -> 'b tyexp

type program = Ty.t tyexp

val string_of_tyexp_custom : ty_to_string:('a -> string) -> 'a tyexp -> string
val string_of_tyexp : program -> string
