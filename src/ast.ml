type exp =
    | BinOp of Lexer.op * exp * exp
    | Variable of Symbol.t
    | IntVal of int

type stmt =
    | Return of exp
    | Assign of Symbol.t * exp

type program = stmt list

let rec string_of_exp = function
    | IntVal x -> Int.to_string x
    | Variable s -> Symbol.str s
    | BinOp (op, lhs, rhs) ->
            let op_str = match op with
                | Plus -> "+"
                | Times -> "*"
                | Divide -> "/"
                | Minus -> "-" in
            let lhs_str = string_of_exp lhs in
            let rhs_str = string_of_exp rhs in
            "(" ^ lhs_str ^ op_str ^ rhs_str ^ ")"

let string_of_stmt = function
    | Return e -> "return " ^ string_of_exp e ^ ";"
    | Assign (l, e) -> Symbol.str l ^ " = " ^ string_of_exp e ^ ";"
