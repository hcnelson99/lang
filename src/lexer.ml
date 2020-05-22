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
    | Var
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
    | Var -> "var"
    | Eof -> "EOF"

type lexer = 
    {
        fname : string;
        file : string;
        length : int;
        mutable idx : int;
        mutable pos : int * int;
        mutable saved_tok : token Mark.t option
    }

let create fname = 
    let contents = In_channel.with_file ~binary:false fname 
        ~f:(fun ch -> In_channel.input_all ch) in
    {
        fname;
        file = contents;
        length = String.length contents;
        idx = 0;
        pos = (1, 1);
        saved_tok = None
    }

let fname {fname; _} = fname

let display_mark lexer m =
    let (r1, c1) = Mark.start m in
    let (r2, c2) = Mark.stop m in
    let lines = String.split lexer.file ~on:'\n' in
    if r1 = r2 then
        let line = List.nth_exn lines (r1 - 1) in
        let repeat s n = List.init n ~f:(fun _ -> s) |> String.concat in
        let underline = repeat " " (c1 - 1) ^ "^" ^ repeat "~" (c2 - c1 - 1) in
        line ^ "\n" ^ underline
    else Printf.sprintf "%d:%d - %d:%d" r1 c1 r2 c2

let peek_char lexer =
    if lexer.idx >= lexer.length then None else
    Some lexer.file.[lexer.idx]

let advance_char lexer = 
    match peek_char lexer with
    | None -> failwith "ICE: Tried to advance lexer at end of file"
    | Some ch ->
            lexer.idx <- lexer.idx + 1;
            let (r, c) = lexer.pos in
            match ch with
            | '\n' -> lexer.pos <- (r + 1, 1)
            | _ -> lexer.pos <- (r, c + 1)

(* let pop_char lexer = *)
(*     let c = peek_char lexer in *)
(*     drop_char lexer; *)
(*     c *)

let eat_token is_valid_char lexer =
    let start_pos = lexer.pos in
    let start_idx = lexer.idx in
    let rec loop () =
        match peek_char lexer with
        | None -> ()
        | Some c -> if is_valid_char c then (advance_char lexer; loop ()) else () in
    advance_char lexer;
    loop ();
    let stop_pos = lexer.pos in
    let stop_idx = lexer.idx in
    let str = String.sub ~pos:start_idx ~len:(stop_idx - start_idx) lexer.file in
    Mark.create str start_pos stop_pos

(* TODO: add lexer error messages *)

let eat_number lexer = 
    let token = eat_token Char.is_digit lexer in
    (match peek_char lexer with
    | None -> ()
    | Some c -> if Char.is_alpha c then failwith "Number cannot end with letter");
    Mark.map ~f:(fun x -> IntVal (Int.of_string x)) token

let is_symbol_start c = Char.is_alpha c || Char.equal c '_'
let is_symbol_char c = Char.is_alphanum c || Char.equal c '_'
let eat_symbol lexer = 
    let symbol = eat_token is_symbol_char lexer in
    Mark.map ~f:(fun s -> match s with
    | "return" -> Return
    | "var" -> Var
    | _ -> Symbol (Symbol.create s)) symbol

let inc (r, c) = (r, c + 1)

(* does not change the saved tok *)
let rec compute_tok lexer = 
    match peek_char lexer with
    | None -> Mark.create Eof lexer.pos (inc lexer.pos)
    | Some c -> 
        if Char.is_whitespace c then (advance_char lexer; compute_tok lexer)
        else if Char.is_digit c then eat_number lexer
        else if is_symbol_start c then eat_symbol lexer
        else 
            let t = match c with
                | '+' -> Operator Plus
                | '-' -> Operator Minus
                | '*' -> Operator Times
                | '/' -> Operator Divide
                | '(' -> LParen
                | ')' -> RParen
                | ';' -> Semicolon
                | '=' -> Equals
                | _ -> failwith "lexer error" in
            let pos = lexer.pos in
            advance_char lexer;
            Mark.create t pos (inc pos)

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
    (pop lexer : token Mark.t) |> ignore

exception Compiler_error

let error lexer mark msg =
    let fname = fname lexer in
    let (r1, c1) = Mark.start mark in
    let (r2, c2) = Mark.stop mark in
    Printf.printf "%s:%d:%d-%d:%d: error: %s\n" fname r1 c1 r2 c2 msg;
    print_endline (display_mark lexer mark);
    raise Compiler_error

