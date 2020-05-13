type operand =
    | IntVal of int
    | Temp of Temp.t

type stmt =
    | BinOp of Temp.t * Lexer.op * operand * operand
    | Assign of Temp.t * operand
    | Return of operand

type program = stmt list

val string_of_operand : operand -> string
val string_of_stmt : stmt -> string
val string_of_program : program -> string

val lower : Ast.program -> program
