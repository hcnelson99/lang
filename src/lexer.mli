type op = Plus | Times | Divide | Minus
type token =
    | IntVal of int
    | Symbol of string
    | Operator of op
    | LParen
    | RParen
    | Semicolon
    | Equals
    | Return
    | Eof

val string_of_token : token -> string

type lexer
val create : string -> lexer
val pop : lexer -> token
val peek : lexer -> token
val drop : lexer -> unit
