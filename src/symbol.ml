open Core

module T = struct
  type t = string [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let of_string x = x
let to_string x = x
