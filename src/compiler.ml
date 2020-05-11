open Core

let lexer = Lexer.create "source"

let main () = 
    print_endline (Ast.string_of_exp (Parser.parse lexer))
