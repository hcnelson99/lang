open Ast
open Core

module L = Lexer

type assoc = Left | Right
let _remove_me = Right

let minimum_precedence = 1

(* precedence * associativity *)
let op_info = function
    | L.Plus | L.Minus -> (5, Left)
    | L.Times | L.Divide -> (6, Left)

let rec parse_atom lexer = 
    match L.pop lexer with
    | L.IntVal x -> IntVal x
    | L.Symbol s -> Variable s
    | L.LParen ->
            let atom = parse_exp minimum_precedence lexer in
            let tok = L.pop lexer in
            if tok <> L.RParen then failwith "unmatched paren";
            atom
    | _ -> failwith "parse error"
and parse_exp min_prec lexer =
    let lhs = parse_atom lexer in
    let rec loop lhs =
        let tok = L.peek lexer in
        match tok with
            | Eof | RParen -> lhs (* should this be anything other than an op? *)
            | Operator op ->
                let prec, assoc = op_info op in
                if prec < min_prec then lhs else
                let () = L.drop lexer in (* eat the op *)
                let rhs_min_prec = match assoc with
                    | Left -> prec + 1
                    | Right -> prec in
                let rhs = parse_exp rhs_min_prec lexer in
                loop (Operator (op, lhs, rhs))
            | _ -> failwith ("not an operator: " ^ L.string_of_token tok)
    in
    loop lhs 

let parse = parse_exp minimum_precedence
