open Core

type t = string [@@deriving sexp, compare, hash, equal]

let of_string x = x
let to_string x = x
