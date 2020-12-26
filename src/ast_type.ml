open Core
module TyVar = Var.MkVar ()

module Mono_ty = struct
  module T = struct
    type t =
      | Var of TyVar.t
      | Int
      | Arrow of t * t
    [@@deriving sexp, compare, hash, equal]
  end

  include T
  include Hashable.Make (T)

  let rec map ~f ty =
    match ty with
    | Var ty -> Var (f ty)
    | Int -> Int
    | Arrow (t1, t2) -> Arrow (map ~f t1, map ~f t2)
  ;;

  let rec free = function
    | Var v -> TyVar.Set.singleton v
    | Int -> TyVar.Set.empty
    | Arrow (t1, t2) -> Set.union (free t1) (free t2)
  ;;

  let rec to_string = function
    (* TODO: Use letters and count up the alphabet nicely *)
    | Var v -> "t" ^ TyVar.to_string v
    | Int -> "int"
    | Arrow (t1, t2) -> "(" ^ to_string t1 ^ " -> " ^ to_string t2 ^ ")"
  ;;
end

module Poly_ty = struct
  (* Do I want to make the Mono case just be Forall with an empty set? *)
  type t =
    | Mono of Mono_ty.t
    | Forall of TyVar.Set.t * Mono_ty.t

  let free = function
    | Mono ty -> Mono_ty.free ty
    | Forall (alpha, ty) -> Set.diff (Mono_ty.free ty) alpha
  ;;

  let to_string = function
    | Mono ty | Forall (_, ty) -> Mono_ty.to_string ty
  ;;
end
