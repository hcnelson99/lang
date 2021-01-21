open! Core

val debug_print_union_find_table : unit -> unit

type t

val to_string : t -> string
val to_string_debug : t -> string
val unconstrained : unit -> t

(* TODO: make this just be
    [ val constructor : constructor -> t list -> t ]
    and have int be arity 0 constructor?
    *)
val arrow : t * t -> t
val int_ : t
val bool_ : t
val is_poly : t -> bool

module Union_find : sig
  exception Unification_error

  val unify : t -> t -> unit

  (* Reinstantiate a polymorphic type *)
  val instantiate : t -> t
end

(* Should only be used after all unification is complete *)
val to_hir_ty : t -> Hir.Ty.t
