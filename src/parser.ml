let parse tokenizer = 
    match Tokenizer.peek tokenizer with
    | Tokenizer.IntVal x -> Ast.IntVal x
    | _ -> failwith "parse error"
