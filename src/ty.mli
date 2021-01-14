open! Core

type t

val to_string : t -> string
val unconstrained : unit -> t

(* TODO: make this just be
    [ val constructor : constructor -> t list -> t ]
    and have int be arity 0 constructor?
    *)
val arrow : t * t -> t
val int_ : t
val is_poly : t -> bool

module Union_find : sig
  exception Unification_error

  val unify : t -> t -> unit

  (* Reinstantiate a polymorphic type *)
  val instantiate : t -> t
end
