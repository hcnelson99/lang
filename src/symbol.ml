open Core

module T = struct
  type t = string [@@deriving sexp, compare, hash, equal]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create x = x
let name x = x
