open Core

type t = string [@@deriving sexp, compare, hash, equal]
let create x = x
let str x = x
