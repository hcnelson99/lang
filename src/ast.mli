type msym = Symbol.t Mark.t

type mexp = exp Mark.t
and exp =
    | BinOp of Lexer.op * mexp * mexp
    | Variable of Symbol.t
    | IntVal of int

type mstmt = stmt Mark.t
and stmt =
    | Return of mexp
    | Assign of msym * mexp

type program = mstmt list

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string
