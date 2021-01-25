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
  | Bool of bool
  | Tuple of 'a tyexp list
  | Split of 'a tyexp * Var.t list * 'a tyexp
  | Ap of 'a tyexp * 'a tyexp
  | Abs of Var.t * 'a tyexp
  | Let of Var.t * 'a tyexp * 'a tyexp

and 'a tyexp = 'a * 'a exp

type program = Ty.t tyexp

val map_ty : f:('a -> 'b) -> 'a tyexp -> 'b tyexp

val format_tyexp_custom
  :  ty_to_string:('a -> string)
  -> Format.formatter
  -> 'a tyexp
  -> unit

val format : Format.formatter -> program -> unit
