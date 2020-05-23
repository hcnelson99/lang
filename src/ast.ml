open Core

type msym = Symbol.t Mark.t

type mexp = exp Mark.t
and exp =
    | BinOp of Lexer.op * mexp * mexp
    | Variable of Symbol.t (* not msym since the Variable is already marked *)
    | IntVal of int
    | BoolVal of bool

type mstmt = stmt Mark.t
and stmt =
    | Return of mexp
    (* | Block of mstmt list *)
    (* | While of mexp * mstmt *)
    (* | If of mexp * mstmt *)
    | Declare of msym
    | Assign of msym * mexp

type program = mstmt list

let rec string_of_exp = function
    | IntVal x -> Int.to_string x
    | BoolVal b -> Bool.to_string b
    | Variable s -> Symbol.str s
    | BinOp (op, lhs, rhs) ->
            let op_str = Lexer.string_of_op op in
            let lhs_str = string_of_exp (Mark.obj lhs) in
            let rhs_str = string_of_exp (Mark.obj rhs) in
            "(" ^ lhs_str ^ op_str ^ rhs_str ^ ")"

let string_of_stmt = function
    | Return e -> "return " ^ string_of_exp (Mark.obj e) ^ ";"
    | Declare s -> "var " ^ Symbol.str (Mark.obj s) ^ ";"
    | Assign (l, e) -> Symbol.str (Mark.obj l) ^ " = " ^ string_of_exp (Mark.obj e) ^ ";"
    (* | Block stmts -> "{" ^ (List.map ~f:(fun x -> string_of_stmt @@ Mark.obj x) stmts |> String.concat ~sep:";\n") ^ "}" *)
    (* | While (cond, body) -> "while(" ^ string_of_exp (Mark.obj cond) ^ ")\n" ^ string_of_stmt (Mark.obj body) *)
    (* | If (cond, body) -> "if(" ^ string_of_exp (Mark.obj cond) ^ ")\n" ^ string_of_stmt (Mark.obj body) *)
