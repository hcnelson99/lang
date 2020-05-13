open Core

let lexer = Lexer.create "source"

let print_stmt stmt = print_endline (Ast.string_of_stmt stmt)

let main () = 
    let ast = Parser.parse lexer in
    Typecheck.typecheck ast;
    List.iter ~f:print_stmt ast;
    let ir = Ir.lower ast in
    print_endline "";
    print_endline (Ir.string_of_program ir)
