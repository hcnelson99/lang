open Core

module S = struct
  type msym = Symbol.t Mark.t [@@deriving sexp]

  (* type binop = Plus *)

  type mexp = exp Mark.t

  and exp =
    | Var of Symbol.t
    | Int of int
    | Bool of bool
    (* | Binop of binop * mexp * mexp *)
    | Tuple of mexp list
    | Ap of mexp * mexp
    | Abs of msym * mexp
    | Let of msym * mexp * mexp
    | Split of mexp * msym list * mexp
  [@@deriving sexp]

  type mty = ty Mark.t

  and ty =
    | Int
    | Bool
    | Tuple of mty list
    | Arrow of mty * mty
  [@@deriving sexp]

  type mstmt = stmt Mark.t

  and stmt =
    | LetStmt of msym * mexp
    | TypeDecl of msym * (msym * mty option) list
  [@@deriving sexp]

  type program = mstmt list [@@deriving sexp]
end
