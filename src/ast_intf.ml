module S = struct
  type msym = Symbol.t Mark.t
  type binop = Plus

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

  type mty = ty Mark.t

  and ty =
    | Int
    | Bool

  type mstmt = stmt Mark.t

  and stmt = LetStmt of msym * mexp

  (* | TypeStmt of msym * (msym * mty option) *)

  type program = mstmt list
end
