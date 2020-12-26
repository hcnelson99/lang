open Core
module TyVar : Var_intf.S

module Mono_ty : sig
  type t =
    | Var of TyVar.t
    | Int
    | Arrow of t * t
  [@@deriving sexp, compare, hash, equal]

  include Hashable.S with type t := t

  val map : f:(TyVar.t -> TyVar.t) -> t -> t
  val free : t -> TyVar.Set.t
  val to_string : t -> string
end

module Poly_ty : sig
  type t =
    | Mono of Mono_ty.t
    | Forall of TyVar.Set.t * Mono_ty.t

  val free : t -> TyVar.Set.t
  val to_string : t -> string
end
