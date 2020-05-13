open Core

type op = Plus | Times | Divide | Minus
type token =
    | IntVal of int
    | Symbol of Symbol.t
    | Operator of op
    | LParen
    | RParen
    | Semicolon
    | Equals
    | Return
    | Eof

let string_of_op = function
    | Plus -> "+"
    | Times -> "*"
    | Divide -> "/"
    | Minus -> "-"

let string_of_token = function
    | IntVal x -> "Int(" ^ Int.to_string x ^ ")" 
    | Symbol s -> "Symbol(" ^ Symbol.str s ^ ")" 
    | Operator op -> string_of_op op
    | LParen -> "("
    | RParen -> ")"
    | Return -> "return"
    | Semicolon -> ";"
    | Equals -> "="
    | Eof -> "EOF"

type lexer = 
    {
        file : string;
        length : int;
        mutable pos : int;
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

let peek_char lexer =
    if lexer.pos >= lexer.length then None else
    Some lexer.file.[lexer.pos]

let drop_char lexer = 
    lexer.pos <- lexer.pos + 1

(* let pop_char lexer = *)
(*     let c = peek_char lexer in *)
(*     drop_char lexer; *)
(*     c *)

let eat_token is_valid_char lexer =
    let start_pos = lexer.pos in
    let rec loop i =
        if i < lexer.length && is_valid_char lexer.file.[i] 
        then loop (i + 1)
        else i in
    let end_pos = loop (start_pos + 1) in
    lexer.pos <- end_pos;
    String.sub ~pos:start_pos ~len:(end_pos - start_pos) lexer.file

let eat_number lexer = 
    let token = eat_token Char.is_digit lexer in
    (match peek_char lexer with
    | None -> ()
    | Some c -> if Char.is_alpha c then failwith "Number cannot end with letter");
    IntVal (Int.of_string token)

let is_symbol_start = Char.is_alpha
let is_symbol_char c = Char.is_alphanum c || c = '_'
let eat_symbol lexer = 
    let symbol = eat_token is_symbol_char lexer in
    match symbol with
    | "return" -> Return
    | _ -> Symbol (Symbol.create symbol)

(* does not change the saved tok *)
let rec compute_tok lexer = 
    match peek_char lexer with
    | None -> Eof
    | Some c -> 
        if Char.is_digit c then eat_number lexer
        else if is_symbol_start c then eat_symbol lexer
        else 
            (drop_char lexer;
            match c with
                | '\n' | '\t' | '\r' | ' ' -> compute_tok lexer
                | '+' -> Operator Plus
                | '-' -> Operator Minus
                | '*' -> Operator Times
                | '/' -> Operator Divide
                | '(' -> LParen
                | ')' -> RParen
                | ';' -> Semicolon
                | '=' -> Equals
                | _ -> failwith "lexer error")

let pop lexer =
    match lexer.saved_tok with
    | Some tok -> lexer.saved_tok <- None; tok
    | None -> compute_tok lexer


let peek lexer =
    match lexer.saved_tok with
    | Some tok -> tok
    | None -> 
        let tok = compute_tok lexer in
        lexer.saved_tok <- Some tok;
        tok

let drop lexer =
    pop lexer |> ignore
