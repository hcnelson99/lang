open! Core

type t

exception Error of t

val error : msg:string -> Lexing.lexbuf -> t
val raise_error : msg:string -> Lexing.lexbuf -> 'a
val error_to_string : t -> string
