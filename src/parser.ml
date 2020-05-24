open Ast
open Core

module L = Lexer

type assoc = Left | Right
let _remove_me = Right

let minimum_precedence = 1

(* precedence * associativity *)
let op_info = function
    | L.Boolean_or -> (1, Left)
    | L.Boolean_and -> (2, Left)
    | L.Equal | L.Not_eq-> (3, Left)
    | L.Greater | L.Greater_eq | L.Less | L.Less_eq -> (4, Left)
    | L.Plus | L.Minus -> (5, Left)
    | L.Times | L.Divide -> (6, Left)

let rec parse_atom lexer = 
    let tok = L.pop lexer in
    match Mark.obj tok with
    | L.True -> Mark.with_mark tok (BoolVal true)
    | L.False -> Mark.with_mark tok (BoolVal false)
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
(* TODO: do we want this to return a list? var x = e1 elaboration should maybe
 * be its own thing? having it here makes the parse logic a little wonky *)
let rec parse_stmt lexer = 
    let tok = L.pop lexer in
    match Mark.obj tok with
    | L.Return -> 
            let e = parse_exp lexer in
            Mark.with_range tok e (Return e)
    | L.Var ->
            let lvalue_tok = L.pop lexer in
            let msym = Mark.map lvalue_tok ~f:(function
                | L.Symbol s -> s
                | _ -> Lexer.error lexer lvalue_tok "Expected lvalue") in
            let eq = L.peek lexer in
            begin match Mark.obj eq with
            | L.Assign -> 
                    L.drop lexer;
                    let e = parse_exp lexer in
                    Mark.with_range tok e (Declare (msym, Some e))
            | L.Semicolon ->
                    (* don't eat semicolon *)
                    Mark.with_range tok lvalue_tok (Declare (msym, None))
            | _ ->  Lexer.error lexer eq "Expected assignment (=)";
            end
    | L.Symbol s ->
            let msym = Mark.with_mark tok s in
            let eq = L.peek lexer in
            begin match Mark.obj eq with
            | L.Assign -> 
                    L.drop lexer;
                    let e = parse_exp lexer in
                    Mark.with_range msym e (Assign (msym, e))
            | _ ->  Lexer.error lexer eq "Expected assignment (=)"
            end
    | L.LBracket ->
            let rec go acc = match Mark.obj (L.peek lexer) with
                | L.RBracket -> (L.pop lexer, (List.rev acc))
                | _ -> 
                    (* TODO: copy paste from below *)
                    let stmt = parse_stmt lexer in
                    let stmt_end = L.pop lexer in
                    begin match Mark.obj stmt_end with
                    | Semicolon -> go (stmt :: acc)
                    | _ -> Lexer.error lexer stmt 
                            "Expected semicolon at end of statement"
                    end in
            let (end_brace, res) = go [] in
            Mark.with_range tok end_brace (Block res)
    | L.While ->
            let l_paren_tok = L.pop lexer in
            begin match (Mark.obj l_paren_tok) with
                | L.LParen -> ()
                | _ -> Lexer.error lexer l_paren_tok "expected ( after while" end;
            let cond = parse_exp lexer in
            let stmt = parse_stmt lexer in
            Mark.with_range tok stmt (While (cond, stmt))
    | L.If ->
            let l_paren_tok = L.pop lexer in
            begin match (Mark.obj l_paren_tok) with
                | L.LParen -> ()
                | _ -> Lexer.error lexer l_paren_tok "expected ( after while" end;
            let cond = parse_exp lexer in
            let stmt = parse_stmt lexer in
            Mark.with_range tok stmt (If (cond, stmt))
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
            | Semicolon -> parse_program (stmt :: acc)
            | _ -> Lexer.error lexer stmt 
                    "Expected semicolon at end of statement"
            end in
    parse_program []
