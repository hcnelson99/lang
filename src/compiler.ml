open Core

let compile lexbuf =
  try Parser.program Lexer.initial lexbuf with
  | Parser.Error -> Error_msg.raise_error ~msg:"Parse error" lexbuf
;;

let main ~fname =
  In_channel.with_file fname ~f:(fun ch ->
      let lexbuf = Lexing.from_channel ch in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
      try
        let ast = compile lexbuf in
        print_s (Ast.sexp_of_program ast);
        let hexp = Typechecker.typecheck ast in
        print_s (Hir.sexp_of_program hexp);
        let hexp_mono = Monomorphize.monomorphize hexp in
        print_s (Hir.sexp_of_program hexp_mono)
      with
      | Error_msg.Error e -> prerr_endline (Error_msg.error_to_string e))
;;
