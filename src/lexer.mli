type op = Plus | Times | Divide | Minus
type token =
    | IntVal of int
    | Symbol of Symbol.t
    | Operator of op
    | LParen
    | RParen
    | Semicolon
    | Equals
    | Return
    | Eof

val string_of_op : op -> string
val string_of_token : token -> string


type lexer

val create : string -> lexer
val fname : lexer -> string
val display_mark : lexer -> 'a Mark.t -> string

val pop : lexer -> token Mark.t
val peek : lexer -> token Mark.t
val drop : lexer -> unit

exception Compiler_error
val error : lexer -> 'a Mark.t -> string -> 'b
