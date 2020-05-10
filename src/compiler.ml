open Core

let tokenizer = Tokenizer.create "source"

let main () = 
    print_endline (Ast.string_of_exp (Parser.parse tokenizer))
