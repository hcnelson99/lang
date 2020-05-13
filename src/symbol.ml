open Core

type t = string [@@deriving sexp, compare, hash]
let create x = x
let str x = x
