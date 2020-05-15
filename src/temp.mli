type t [@@deriving sexp, compare, hash, equal]
val create : unit -> t
val string_of_t : t -> string

(* temporary before we finish register allocation? *)
val number : t -> int

(* temporary before we finish register allocation *)
val max_temp_hack : unit -> int
