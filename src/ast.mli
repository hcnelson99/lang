type exp =
    | Operator of Lexer.op * exp * exp
    | Variable of string
    | IntVal of int

type stmt =
    | Return of exp
    | Assign of string * exp

type program = stmt list

val string_of_exp : exp -> string
val string_of_stmt : stmt -> string
