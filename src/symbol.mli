type t [@@deriving sexp, compare, hash, equal]
val create : string -> t
val str : t -> string
