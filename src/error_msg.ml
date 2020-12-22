open! Core

type t = Lexing.position * Lexing.position * string

exception Error of t

let error ~msg lexbuf = Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf, msg

let raise_error ~msg lexbuf =
  raise (Error (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf, msg))
;;

let error_to_string (start, _stop, msg) =
  let open Lexing in
  let fname = start.pos_fname in
  let line = start.pos_lnum in
  let col = start.pos_cnum - start.pos_bol in
  [%string "%{fname}:%{line#Int}:%{col#Int}: %{msg}"]
;;
