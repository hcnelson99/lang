open Core

type t = int [@@deriving sexp, compare, hash]

let next_temp = ref 1

let create () = 
    let res = !next_temp in
    next_temp := !next_temp + 1;
    res

let string_of_t t = "%t" ^ Int.to_string t
