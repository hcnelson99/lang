open Core

type reg = 
    | RAX | RBX | RCX | RDX | RSI | RDI | R8 
    | R9 | R10 | R11 | R12 | R13 | R14 | R15 [@@deriving sexp, compare, hash, equal]

let all_regs = [RAX; RBX; RCX; RDX; RSI; RDI; R8; R9; R10; R11; R12; R13; R14; R15]

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

(* TODO: support spilling ... ? *)

let string_of_temp_lvalue = function
    | Reg r -> string_of_reg r
    | Temp s -> Temp.string_of_t s

let string_of_reg_lvalue = function
    | Reg r -> string_of_reg r
    | Temp s -> string_of_reg s
    
let string_of_operand sol = function
    | Imm i -> "$" ^ Int.to_string i
    | LValue l -> sol l

let string_of_two_op = function
    | Imul -> "imulq"
    | Add -> "addq"
    | Sub -> "subq"

let string_of_stmt sol = function
    | TwoAddr (two_op, op, lvalue) -> string_of_two_op two_op ^ " " ^ string_of_operand sol op ^ ", " ^ sol lvalue
    | Mov (op, lvalue) -> "movq " ^ string_of_operand sol op ^ ", " ^ sol lvalue

let string_of_program program =
    program
    |> List.map ~f:(string_of_stmt string_of_reg_lvalue)
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


(* return (stmt * creations * expired) list *)
let compute_live_ranges asm : (Temp.t stmt * LValueTemp.t list * LValueTemp.t list) list =
    let active = Hash_set.create (module LValueTemp) in
    let rev_asm = List.rev asm in
    let operand_to_lvalue = function
        | Imm _ -> []
        | LValue x -> [x] in
    let get_defines = function
        | TwoAddr(_, _, lvalue) -> [lvalue]
        | Mov (_, lvalue) -> [lvalue] in
    let get_uses = function
        | TwoAddr(_, op, lvalue) -> lvalue :: operand_to_lvalue op 
        | Mov (op, _) -> operand_to_lvalue op in
    let rec go acc = function
        | [] -> acc
        | stmt :: stmts -> 
                (* TODO: this assumes everything will be defined only once? not totally sure about this... *)
                let (defines, uses) = (get_defines stmt, get_uses stmt) in
                (* things that were defined on this line but not used are creations *)
                let creations = List.filter defines 
                    ~f:(fun x -> not (List.mem uses x ~equal:LValueTemp.equal)) in
                List.iter creations ~f:(fun x -> 
                    Hash_set.remove active x
                );
                (* add uses if they don't already exist *)
                (* something expires here if we are adding it to the active list for the first time *)
                let expires = List.concat_map uses ~f:(fun x -> 
                    if Hash_set.mem active x then []
                    else (Hash_set.add active x; [x])
                ) in
                go ((stmt, creations, expires) :: acc) stmts in
    let res = go [] rev_asm in
    let p_tl = List.to_string ~f:string_of_temp_lvalue in
    List.iter res ~f:(fun (stmt, creations, expires) ->
        Printf.printf "%s (created: %s, expired: %s\n" (string_of_stmt string_of_temp_lvalue stmt) (p_tl creations) (p_tl expires));
    res

let register_allocate (asm : Temp.t program) =
    let live_ranges_asm = compute_live_ranges asm in
    let register_mapping = Hashtbl.create (module Temp) in
    (* TODO: better way of handling free registers, spilling? *)
    let free_regs = ref all_regs in
    let fresh_reg () = match !free_regs with
        | [] -> failwith "ran out of registers"
        | reg :: regs -> free_regs := regs; reg in
    let use_phys_reg reg =
        let free_regs' = List.filter !free_regs ~f:(fun x -> not (equal_reg x reg)) in
        if List.length !free_regs = List.length free_regs' then 
            failwith "was required to use same physical register simultaneously";
        free_regs := free_regs' in
    let free_reg reg =
        if List.mem !free_regs reg ~equal:equal_reg then failwith "reg was already free";
        free_regs := reg :: !free_regs in
    let expire t =
        let reg = match t with
            | Reg r -> r
            | Temp t -> Hashtbl.find_exn register_mapping t in
        free_reg reg;
        in
    let rec go acc = function
        | [] -> List.rev acc
        | (stmt, creations, expires)  :: ranges ->
                List.iter expires ~f:expire;
                List.iter creations ~f:(fun new_reg -> match new_reg with
                    | Reg r -> use_phys_reg r
                    | Temp t -> Hashtbl.add_exn register_mapping ~key:t ~data:(fresh_reg ()));
                let map_lvalue = function
                    | Reg r -> Reg r
                    | Temp t -> Temp (Hashtbl.find_exn register_mapping t) in
                let map_operand = function
                    | Imm i -> Imm i
                    | LValue l -> LValue (map_lvalue l) in
                let map_stmt = function
                    | TwoAddr (two_op, o, l) -> TwoAddr (two_op, map_operand o, map_lvalue l)
                    | Mov (o, l) -> Mov(map_operand o, map_lvalue l) in
                go (map_stmt stmt :: acc) ranges
    in
    go [] live_ranges_asm
