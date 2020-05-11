open Core

type op = Plus | Times | Divide | Minus
type token =
    | IntVal of int
    | Operator of op
    | LParen
    | RParen
    | Eof

let string_of_token = function
    | IntVal x -> "Int(" ^ Int.to_string x ^ ")" 
    | Operator Plus -> "+"
    | Operator Times -> "*"
    | Operator Divide -> "/"
    | Operator Minus -> "-"
    | LParen -> "("
    | RParen -> ")"
    | Eof -> "EOF"

type lexer = 
    {
        file : string;
        length : int;
        mutable pos : int;

        (* used to allow peeking. right now you can only peek one token, so
         * this is an option (aka a list of length 1) *)
        mutable saved_tok : token option
    }

let create fname = 
    let contents = In_channel.with_file ~binary:false fname 
        ~f:(fun ch -> In_channel.input_all ch) in
    {
        file = contents;
        length = String.length contents;
        pos = 0;
        saved_tok = None
    }

exception LexerEof
let advance lexer = 
    if lexer.pos >= lexer.length then raise LexerEof else
    lexer.pos <- lexer.pos + 1

(* does not change the saved tok *)
let rec compute_tok lexer = 
    let pos = lexer.pos in
    if pos = lexer.length then Eof else
    let c = lexer.file.[pos] in
    let () = advance lexer in
    match c with
    | '0' .. '9' -> IntVal (Int.of_string (String.of_char c))
    | '\n' | '\t' | '\r' | ' ' -> compute_tok lexer
    | '+' -> Operator Plus
    | '-' -> Operator Minus
    | '*' -> Operator Times
    | '/' -> Operator Divide
    | '(' -> LParen
    | ')' -> RParen
    | _ -> failwith "lexer error"

let pop lexer =
    let temp = 
        match lexer.saved_tok with
        | Some tok -> lexer.saved_tok <- None; tok
        | None -> compute_tok lexer in
    let () = print_endline ("popping " ^  string_of_token temp) in
    temp


let peek lexer =
    match lexer.saved_tok with
    | Some tok -> tok
    | None -> 
        let tok = compute_tok lexer in
        lexer.saved_tok <- Some tok;
        tok

let drop lexer =
    pop lexer |> ignore
