open Core

type reg = 
    | RAX | RBX | RCX | RDX | RSI | RDI | R8 
    | R9 | R10 | R11 | R12 | R13 | R14 | R15

(* first six args *)
let _arg_regs = [RDI; RSI; RDX; RCX; R8; R9]

let _caller_save = [RAX; RCX; RDX; RSI; RDI; R8; R9; R10; R11]

(* also rsp, rbp *)
let _callee_save = [RBX; R12; R13; R14; R15]

type 'a lvalue = Reg of reg | Temp of 'a

type 'a operand =
    | Imm of int
    | LValue of 'a lvalue

type two_op = Imul | Add | Sub

type 'a stmt =
    | TwoAddr of two_op * 'a operand * 'a lvalue
    | Mov of 'a operand * 'a lvalue

type 'a program = 'a stmt list

let string_of_reg = function
    | RAX -> "%rax"
    | RBX -> "%rbx"
    | RCX -> "%rcx"
    | RDX -> "%rdx"
    | RSI -> "%rsi"
    | RDI -> "%rdi"
    | R8  -> "%r8"
    | R9  -> "%r9"
    | R10 -> "%r10"
    | R11 -> "%r11"
    | R12 -> "%r12"
    | R13 -> "%r13"
    | R14 -> "%r14"
    | R15 -> "%r15"

let string_of_lvalue = function
    | Reg r -> string_of_reg r
    (* TODO: make this print for ints instead of Temp.ts *)
    | Temp s -> Printf.sprintf "-%d(%%rbp)" (8 * Temp.number s)
    
let string_of_operand = function
    | Imm i -> "$" ^ Int.to_string i
    | LValue l -> string_of_lvalue l

let string_of_two_op = function
    | Imul -> "imulq"
    | Add -> "addq"
    | Sub -> "subq"

let string_of_stmt = function
    | TwoAddr (two_op, op, lvalue) -> string_of_two_op two_op ^ " " ^ string_of_operand op ^ ", " ^ string_of_lvalue lvalue
    | Mov (op, lvalue) -> "movq " ^ string_of_operand op ^ ", " ^ string_of_lvalue lvalue

let string_of_program program =
    program
    |> List.map ~f:(fun s -> "  " ^ string_of_stmt s) 
    |> String.concat ~sep:"\n"

let lower_to_x86 ir =
    let lower_operand = function
        | Ir.IntVal i -> Imm i
        | Ir.Temp t -> LValue (Temp t) in
    let lower_binop = function
        | Lexer.Plus -> Add
        | Lexer.Times -> Imul
        | Lexer.Minus -> Sub
        | Lexer.Divide -> failwith "divide not supported" in
    let lower_stmt = function
        | Ir.Return op -> [Mov (lower_operand op, Reg RAX)]
        | Ir.Assign (t, op) -> [Mov (lower_operand op, Temp t)]
        | Ir.BinOp (t, binop, op1, op2) -> 
                (* TODO use commutativity to reduce copies *)
                let op = lower_binop binop in
                [Mov (lower_operand op1, Temp t);
                 TwoAddr (op, lower_operand op2, Temp t)] in
    List.concat_map ~f:lower_stmt ir
