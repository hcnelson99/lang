open! Core

type t

module Constructor : sig
  type t =
    | Int
    | Bool
    | Arrow
    | Tuple
  [@@deriving equal]
end

val unconstrained : unit -> t
val constructor : Constructor.t -> t list -> t
val is_poly : t -> bool

module Union_find : sig
  exception Unification_error

  val unify : t -> t -> unit

  (* Reinstantiate a polymorphic type *)
  val instantiate : t -> t
end

(* Should only be used after all unification is complete *)
val to_hir_ty : t -> Hir.Ty.t
