{
open Core

module T = Parser
}

let ident = ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_']*
let num = ("0" | ['1'-'9'](['0'-'9']*))
let ws = [' ' '\t' '\r' '\011' '\012']

rule initial = parse
    | ws+ { initial lexbuf }
    | '\n' { 
        Lexing.new_line lexbuf;
        initial lexbuf 
    }
    | '(' { T.LParen }
    | ')' { T.RParen }
    | '=' { T.Equal }
    | "->" { T.Arrow }
    | "fun" { T.Fun }
    | "let" { T.Let }
    | "in" { T.In }
    | ident as name { T.Ident (Symbol.of_string name) }
    | num as n { T.Int_literal (Int.of_string n) }
    | eof { T.Eof }
    | _ { Error_msg.raise_error ~msg:"Illegal character" lexbuf }
