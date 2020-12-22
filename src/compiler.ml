open Core

exception Error

let main () = 
    let lexbuf = Lexing.from_string "123" in
    let program = 
          try Parser.program Lexer.initial lexbuf  with 
          | Parser.MenhirBasics.Error -> (prerr_endline "error"; raise Error)
    in
    print_endline (Ast.string_of_program program)
