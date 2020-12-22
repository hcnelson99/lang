type t [@@deriving sexp, compare, hash, equal]

val of_string : string -> t
val to_string : t -> string
