type op = Plus | Times | Divide | Minus
type token =
    | IntVal of int
    | Operator of op
    | LParen
    | RParen
    | Eof

type tokenizer
val create : string -> tokenizer
val peek : tokenizer -> token
