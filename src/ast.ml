type exp =
    | Operator of Lexer.op * exp * exp
    | Variable of string
    | IntVal of int

let rec string_of_exp = function
    | IntVal x -> Int.to_string x
    | Variable s -> s
    | Operator (op, lhs, rhs) ->
            let op_str = match op with
                | Plus -> "+"
                | Times -> "*"
                | Divide -> "/"
                | Minus -> "-" in
            let lhs_str = string_of_exp lhs in
            let rhs_str = string_of_exp rhs in
            "(" ^ lhs_str ^ op_str ^ rhs_str ^ ")"
