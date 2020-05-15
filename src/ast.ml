type msym = Symbol.t Mark.t

type mexp = exp Mark.t
and exp =
    | BinOp of Lexer.op * mexp * mexp
    | Variable of Symbol.t (* not msym since the Variable is already marked *)
    | IntVal of int

type mstmt = stmt Mark.t
and stmt =
    | Return of mexp
    | Assign of msym * mexp

type program = mstmt list

let rec string_of_exp = function
    | IntVal x -> Int.to_string x
    | Variable s -> Symbol.str s
    | BinOp (op, lhs, rhs) ->
            let op_str = match op with
                | Plus -> "+"
                | Times -> "*"
                | Divide -> "/"
                | Minus -> "-" in
            let lhs_str = string_of_exp (Mark.obj lhs) in
            let rhs_str = string_of_exp (Mark.obj rhs) in
            "(" ^ lhs_str ^ op_str ^ rhs_str ^ ")"

let string_of_stmt = function
    | Return e -> "return " ^ string_of_exp (Mark.obj e) ^ ";"
    | Assign (l, e) -> Symbol.str (Mark.obj l) ^ " = " ^ string_of_exp (Mark.obj e) ^ ";"
