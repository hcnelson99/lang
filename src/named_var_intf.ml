open Core

module type S = sig
  type t [@@deriving compare, hash, sexp]

  include Hashable.S with type t := t
  include Comparable.S with type t := t

  val create : string -> t
  val to_string : t -> string
  val to_string_hum : t -> string
  val name : t -> string
end
