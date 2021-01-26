open! Core

type t

val to_string : t -> string

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
  val unify : t -> t -> unit

  (* Reinstantiate a polymorphic type *)
  val instantiate : t -> t
end

(* Should only be used after all unification is complete *)
val to_hir_ty : t -> Hir.Ty.t
