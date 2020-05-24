type msym = Symbol.t Mark.t

type mexp = exp Mark.t
and exp =
    | BinOp of Lexer.op * mexp * mexp
    | Variable of Symbol.t
    | IntVal of int
    | BoolVal of bool

type mstmt = stmt Mark.t
and stmt =
    | Return of mexp
    | Block of mstmt list
    | While of mexp * mstmt
    | If of mexp * mstmt
    | Declare of msym * mexp option
    | Assign of msym * mexp

type program = mstmt list

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string
