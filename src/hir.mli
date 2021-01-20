open Core

module Ty : sig
  module Var : Uid_intf.S

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

module Var : sig
  type t [@@deriving compare, hash, sexp]

  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val create : string -> t
  val to_string : t -> string
  val name : t -> string
end

type 'a exp =
  | Var of Var.t
  | Int of int
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Var.t * 'a tyexp
  | Let of Var.t * 'a tyexp * 'a tyexp

and 'a tyexp = 'a * 'a exp

val map_ty : f:('a -> 'b) -> 'a tyexp -> 'b tyexp

type program = Ty.t tyexp

val string_of_tyexp_custom : ty_to_string:('a -> string) -> 'a tyexp -> string
val string_of_tyexp : program -> string
