type exp =
    | Operator of Lexer.op * exp * exp
    | Variable of Symbol.t
    | IntVal of int

type stmt =
    | Return of exp
    | Assign of Symbol.t * exp

type program = stmt list

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string
