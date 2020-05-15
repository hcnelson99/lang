open Core

let lexer = Lexer.create "source"

(* let print_stmt stmt = print_endline (Ast.string_of_stmt stmt) *)

let system cmd =
    (Unix.system cmd  : Unix.Exit_or_signal.t)
    |> ignore

let main () = 
    let ast = (try 
        let ast = Parser.parse lexer in
        Typecheck.typecheck lexer ast;
        ast
    with Lexer.Compiler_error -> Unix.exit_immediately 1) in
    (* List.iter ~f:print_stmt ast; *)
    let ir = Ir.lower ast in
    (* print_endline ""; *)
    print_endline (Ir.string_of_program ir);
    let asm = Codegen.emit ir in
    (* print_endline asm *)
    Out_channel.with_file "out.s" ~f:(fun out -> Out_channel.output_string out asm);
    system "gcc -c out.s -o out.o";
    system "gcc out.o runtime.c -o out";
    system "./out"
