open Core

type t [@@deriving sexp, compare, hash, equal]

include Comparable.S with type t := t
include Hashable.S with type t := t

val create : string -> t
val name : t -> string
