type exp =
    | Operator of Tokenizer.op * exp * exp
    | IntVal of int

let string_of_exp = function
    | IntVal x -> Int.to_string x
    | _ -> failwith "string_of_exp"
