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
            let atom = parse_exp lexer in
            let tok = L.pop lexer in
            if tok <> L.RParen then failwith "unmatched paren";
            atom
    | _ -> failwith "parse error"
and parse_exp lexer =
    let rec parse_exp' min_prec = 
        let lhs = parse_atom lexer in
        let rec loop lhs =
            let tok = L.peek lexer in
            match tok with
                (* seems kinda sad to have to put all the expression-ending
                 * tokens here. but it also kind of makes sense? *)
                | Eof | RParen | Semicolon -> lhs
                | Operator op ->
                    let prec, assoc = op_info op in
                    if prec < min_prec then lhs else
                    let () = L.drop lexer in (* eat the op *)
                    let rhs_min_prec = match assoc with
                        | Left -> prec + 1
                        | Right -> prec in
                    let rhs = parse_exp' rhs_min_prec in
                    loop (BinOp (op, lhs, rhs))
                | _ -> failwith ("not an operator: " ^ L.string_of_token tok) in
        loop lhs in
    parse_exp' minimum_precedence

(* doesnt eat the semicolon *)
let parse_stmt lexer = 
    match L.pop lexer with
    | L.Return -> Return (parse_exp lexer)
    | L.Symbol lvalue ->
            let tok = L.pop lexer in
            if tok <> L.Equals then failwith "Expected assignment (=)";
            Assign(lvalue, parse_exp lexer)
    | _ -> failwith "Illegal token to begin statement"

let parse lexer =
    let rec parse_program acc =
        match L.peek lexer with
        | Eof -> List.rev acc
        | _ -> 
            let stmt = parse_stmt lexer in
            if L.pop lexer <> Semicolon then failwith "Expected semicolon";
            parse_program (stmt::acc) in
    parse_program []
