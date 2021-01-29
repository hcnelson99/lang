type pos = Lexing.position
type 'a t [@@deriving sexp]

val start : 'a t -> pos
val stop : 'a t -> pos
val obj : 'a t -> 'a
val map : 'a t -> f:('a -> 'b) -> 'b t
val with_mark : 'a t -> 'b -> 'b t
val create : 'a -> pos -> pos -> 'a t
val with_range : 'a t -> 'b t -> 'c -> 'c t
