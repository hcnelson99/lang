open Core

let lexer = Lexer.create "source"

let print_stmt stmt = print_endline (Ast.string_of_stmt stmt)

let main () = 
    let program = Parser.parse lexer in
    Typecheck.typecheck program;
    List.iter ~f:print_stmt program
