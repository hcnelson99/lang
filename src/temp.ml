open Core

type t = int [@@deriving sexp, compare, hash, equal]

let next_temp = ref 1

let create () = 
    let res = !next_temp in
    next_temp := !next_temp + 1;
    res

(* temporary before we finish register allocation? *)
let number t = t

let string_of_t t = "%t" ^ Int.to_string t

let max_temp_hack () = !next_temp
