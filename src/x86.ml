open Core

type reg = 
    | RAX | RBX | RCX | RDX | RSI | RDI | R8 
    | R9 | R10 | R11 | R12 | R13 | R14 | R15 [@@deriving sexp, compare, hash, equal]

(* first six args *)
let _arg_regs = [RDI; RSI; RDX; RCX; R8; R9]

let _caller_save = [RAX; RCX; RDX; RSI; RDI; R8; R9; R10; R11]

(* also rsp, rbp *)
let _callee_save = [RBX; R12; R13; R14; R15]

type 'a lvalue = Reg of reg | Temp of 'a [@@deriving sexp, compare, hash, equal]

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
    |> List.mapi ~f:(fun i s -> "  " ^ string_of_stmt s ^ "\t// " ^ Int.to_string i) 
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
    type t = Temp.t lvalue [@@deriving sexp, compare, hash, equal]
end

let liveness ir =
    let active = Hashtbl.create (module LValueTemp) in
    let ranges = Hashtbl.create (module LValueTemp) in
    let n = List.length ir in
    let rev_ir = List.rev ir in
    let operand_to_lvalue = function
        | Imm _ -> []
        | LValue x -> [x] in
    let get_defines = function
        | TwoAddr(_, _, lvalue) -> [lvalue]
        | Mov (_, lvalue) -> [lvalue] in
    let get_uses = function
        | TwoAddr(_, op, lvalue) -> lvalue :: operand_to_lvalue op 
        | Mov (op, _) -> operand_to_lvalue op in
    let rec go i = function
        | [] -> ()
        | stmt :: stmts -> 
                let (defines, uses) = (get_defines stmt, get_uses stmt) in
                let defines_not_used = List.filter defines 
                    ~f:(fun x -> not (List.mem uses x ~equal:LValueTemp.equal)) in
                (* remove defines that are not also used *)
                List.iter defines_not_used ~f:(fun x -> 
                    match Hashtbl.find_and_remove active x with
                    | Some e -> Hashtbl.add_exn ranges ~key:x ~data:(i, e)
                    | None -> () (* must have been a define that was never used *));
                (* add uses if they don't already exist *)
                List.iter uses ~f:(fun x -> Hashtbl.add active ~key:x ~data:i |> ignore);
                go (i-1) stmts in
    go (n - 1) rev_ir;
    Hashtbl.to_alist ranges
    |> List.map ~f:(fun (l, (s, e)) -> Printf.sprintf "%s: %d, %d" (string_of_lvalue l) s e)
    |> String.concat ~sep:"\n"
    |> print_endline

