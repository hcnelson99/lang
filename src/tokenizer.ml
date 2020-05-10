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

exception TokenizerEof
let advance tokenizer = 
    if tokenizer.pos >= tokenizer.length then raise TokenizerEof else
    tokenizer.pos <- tokenizer.pos + 1

(* does not change the saved tok *)
let rec compute_tok tokenizer = 
    let pos = tokenizer.pos in
    if pos = tokenizer.length then Eof else
    let c = tokenizer.file.[pos] in
    let () = advance tokenizer in
    match c with
    | '0' .. '9' -> IntVal (Int.of_string (String.of_char c))
    | '\n' | '\t' | '\r' | ' ' -> compute_tok tokenizer
    | '+' -> Operator Plus
    | '-' -> Operator Minus
    | '*' -> Operator Times
    | '/' -> Operator Divide
    | _ -> failwith "tokenizer error"

let pop tokenizer =
    match tokenizer.saved_tok with
    | Some tok -> tokenizer.saved_tok <- None; tok
    | None -> compute_tok tokenizer

let peek tokenizer =
    match tokenizer.saved_tok with
    | Some tok -> tok
    | None -> 
        let tok = compute_tok tokenizer in
        tokenizer.saved_tok <- Some tok;
        tok
