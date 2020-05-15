open Core

type reg = 
    | RAX | RBX | RCX | RDX | RSI | RDI | R8 
    | R9 | R10 | R11 | R12 | R13 | R14 | R15 [@@deriving sexp, compare, hash]

(* first six args *)
let _arg_regs = [RDI; RSI; RDX; RCX; R8; R9]

let _caller_save = [RAX; RCX; RDX; RSI; RDI; R8; R9; R10; R11]

(* also rsp, rbp *)
let _callee_save = [RBX; R12; R13; R14; R15]

type 'a lvalue = Reg of reg | Temp of 'a [@@deriving sexp, compare, hash]

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
    | Temp s -> "%t" ^ Int.to_string (Temp.number s)
    
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

let lower_to_two_address ir =
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

module LValueTemp = struct
    type t = Temp.t lvalue [@@deriving sexp, compare, hash]
end

let liveness ir =
    let active = Hash_set.create (module LValueTemp) () in
    let rev_ir = List.rev ir in
    let operand_to_lvalue = function
        | Imm _ -> []
        | LValue x -> [x] in
    let defines = function
        | TwoAddr(_, _, lvalue) -> [lvalue]
        | Mov (_, lvalue) -> [lvalue] in
    let uses = function
        | TwoAddr(_, op, lvalue) -> lvalue :: operand_to_lvalue op 
        | Mov (op, _) -> operand_to_lvalue op in
    let rec go = function
        | [] -> ()
        | i :: is -> 
                active
                |> Hash_set.to_list
                |> List.map ~f:string_of_lvalue
                |> String.concat ~sep:" "
                |> (fun x -> "  " ^ x)
                |> print_endline;
                print_endline (string_of_stmt i);
                let (d, u) = (defines i, uses i) in
                List.iter d ~f:(fun x -> Hash_set.remove active x);
                List.iter u ~f:(fun x -> Hash_set.add active x);
                go is in
    go rev_ir

