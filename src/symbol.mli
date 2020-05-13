type t [@@deriving sexp, compare, hash]
val create : string -> t
val str : t -> string
