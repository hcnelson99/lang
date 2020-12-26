open Core

module type S = sig
  type t [@@deriving sexp, compare, hash, equal]

  include Comparable.S with type t := t
  include Hashable.S with type t := t

  val create : unit -> t
  val to_string : t -> string
end
