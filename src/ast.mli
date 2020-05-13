type exp =
    | Operator of Lexer.op * exp * exp
    | Variable of string
    | IntVal of int

val string_of_exp : exp -> string
