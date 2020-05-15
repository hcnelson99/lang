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
    let tok = L.pop lexer in
    match Mark.obj tok with
    | L.IntVal x -> Mark.with_mark tok (IntVal x)
    | L.Symbol s -> Mark.with_mark tok (Variable s)
    | L.LParen ->
            let atom = parse_exp lexer in
            let close = L.pop lexer in
            Mark.with_range tok close (match Mark.obj close with
            | L.RParen -> Mark.obj atom
            | _ -> Lexer.error lexer close "Expected closing parenthesis"
            )
    | _ -> Lexer.error lexer tok "Expected beginning of expression"
and parse_exp lexer =
    let rec parse_exp' min_prec = 
        let lhs = parse_atom lexer in
        let rec loop lhs =
            let tok = L.peek lexer in
            match Mark.obj tok with
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
                    loop (Mark.with_range lhs rhs (BinOp (op, lhs, rhs)))
                | _ -> Lexer.error lexer tok "not an operator" in
        loop lhs in
    parse_exp' minimum_precedence

(* doesnt eat the semicolon *)
let parse_stmt lexer = 
    let tok = L.pop lexer in
    match Mark.obj tok with
    | L.Return -> 
            let e = parse_exp lexer in
            Mark.with_range tok e (Return e)
    | L.Symbol lvalue ->
            let eq = L.pop lexer in
            begin match Mark.obj eq with
            | L.Equals -> 
                    let e = parse_exp lexer in
                    Mark.with_range tok e (Assign (Mark.with_mark tok lvalue, e))
            | _ ->  Lexer.error lexer eq "Expected assignment (=)";
            end
    | _ -> Lexer.error lexer tok "Unexpected token to begin statement"

let parse lexer =
    let rec parse_program acc =
        let tok = L.peek lexer in
        match Mark.obj tok with
        | Eof -> List.rev acc
        | _ -> 
            let stmt = parse_stmt lexer in
            let stmt_end = L.pop lexer in
            begin match Mark.obj stmt_end with
            | Semicolon -> parse_program (stmt::acc)
            | _ -> Lexer.error lexer stmt_end "Expected semicolon at end of statement"
            end in
    parse_program []
