type op = 
    | Plus | Times | Divide | Minus | Greater | Less | Equal | Greater_eq
    | Less_eq | Not_eq 
    | Boolean_and | Boolean_or

type token =
    | IntVal of int
    | Symbol of Symbol.t
    | Operator of op
    | LParen
    | RParen
    | LBracket
    | RBracket
    | Semicolon
    | Assign
    | Return
    | True
    | False
    | While
    | If
    | Var
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
