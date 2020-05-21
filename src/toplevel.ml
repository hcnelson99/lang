open Core

let system cmd =
    (Unix.system cmd  : Unix.Exit_or_signal.t)
    |> ignore

let system_retval cmd =
    match Unix.system cmd with
    | Ok () -> Some 0
    | Error (`Exit_non_zero c) -> Some c
    | Error (`Signal _) -> None

let compile_and_exec fname = 
    let lexer = Lexer.create fname in
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
    system_retval "./out"


