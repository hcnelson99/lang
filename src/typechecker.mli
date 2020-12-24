module Poly_ty : sig
  type t

  val to_string : t -> string
end

val typecheck : Ast.program -> Poly_ty.t
