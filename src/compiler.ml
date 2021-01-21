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
        Caml.Format.printf "%a@." Ast.format ast;
        let hexp = Typechecker.typecheck ast in
        Caml.Format.printf "%a@." Hir.format hexp;
        let hexp_mono = Monomorphize.monomorphize hexp in
        Caml.Format.printf "%a@." Hir.format hexp_mono
      with
      | Error_msg.Error e -> prerr_endline (Error_msg.error_to_string e))
;;
