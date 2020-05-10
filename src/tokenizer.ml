open Core

type op = Plus | Times | Divide | Minus
type token =
    | IntVal of int
    | Operator of op
    | LParen
    | RParen
    | Eof


type tokenizer = 
    {
        file : string;
        length : int;
        mutable pos : int;
    }

let create fname = 
    let contents = In_channel.with_file ~binary:false fname 
        ~f:(fun ch -> In_channel.input_all ch) in
    {
        file = contents;
        length = String.length contents;
        pos = 0;
    }

exception TokenizerEof
let advance tokenizer = 
    if tokenizer.pos >= tokenizer.length then raise TokenizerEof else
    tokenizer.pos <- tokenizer.pos + 1

let rec peek tokenizer = 
    let pos = tokenizer.pos in
    if pos = tokenizer.length then Eof else
    let c = tokenizer.file.[pos] in
    let () = advance tokenizer in
    match c with
    | '0' .. '9' -> IntVal (Int.of_string (String.of_char c))
    | '\n' | '\t' | '\r' | ' ' -> peek tokenizer
    | '+' -> Operator Plus
    | '-' -> Operator Minus
    | '*' -> Operator Times
    | '/' -> Operator Divide
    | _ -> failwith "tokenizer error"
