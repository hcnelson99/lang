open Core

module A = Ast

type operand =
    | IntVal of int
    | Temp of Temp.t

type stmt =
    | BinOp of Temp.t * Lexer.op * operand * operand
    | Assign of Temp.t * operand
    | Return of operand

type program = stmt list

let string_of_operand = function
    | IntVal i -> Int.to_string i
    | Temp t -> Temp.string_of_t t

let string_of_stmt = function
    | BinOp (lhs, binop, op1, op2) -> 
            let lhs = Temp.string_of_t lhs in
            let binop = Lexer.string_of_op binop in
            let op1 = string_of_operand op1 in
            let op2 = string_of_operand op2 in
            lhs ^ " = " ^ op1 ^ " " ^ binop ^ " " ^ op2
    | Assign (lhs, operand) -> 
            Temp.string_of_t lhs ^ " = " ^ string_of_operand operand
    | Return e -> "return " ^ string_of_operand e

let string_of_program program =
    List.map ~f:(fun s -> string_of_stmt s ^ "\n") program
    |> String.concat

(* TODO: generate fewer temps *)
let lower program = 
    let symbols = Hashtbl.create (module Symbol) in
    let lookup s = match Hashtbl.find symbols s with
        | None -> failwith "ICE: variable used before definition"
        | Some t -> t in
    let assign s t = Hashtbl.add symbols ~key:s ~data:t |> ignore in
    let rec lower_exp (dst : Temp.t) = function
        | A.IntVal i -> [Assign(dst, IntVal i)]
        | A.BinOp (op, e1, e2) -> 
                let lhs = Temp.create () in
                let rhs = Temp.create () in
                lower_exp lhs e1 @
                lower_exp rhs e2 @
                [BinOp (dst, op, Temp lhs, Temp rhs) ]
        | A.Variable s -> [Assign (dst, Temp (lookup s))] in
    let lower_stmt = function
        | A.Return e ->
                let t = Temp.create () in
                lower_exp t e @
                [ Return (Temp t) ]
        | A.Assign (lhs, e) ->
                let t = Temp.create () in
                let stmts = lower_exp t e in
                assign lhs t; (* lhs is t from now on *)
                stmts in
    List.concat_map ~f:lower_stmt program
