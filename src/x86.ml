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

let string_of_lvalue = function
    | Reg r -> string_of_reg r
    | Temp s -> string_of_reg s
    
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
    |> List.map ~f:string_of_stmt
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

let compute_live_ranges asm =
    let active = Hashtbl.create (module LValueTemp) in
    let live_ranges = Hashtbl.create (module LValueTemp) in
    let n = List.length asm in
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
    let rec go i = function
        | [] -> live_ranges
        | stmt :: stmts -> 
                let (defines, uses) = (get_defines stmt, get_uses stmt) in
                let defines_not_used = List.filter defines 
                    ~f:(fun x -> not (List.mem uses x ~equal:LValueTemp.equal)) in
                (* remove defines that are not also used *)
                List.iter defines_not_used ~f:(fun x -> 
                    match Hashtbl.find_and_remove active x with
                    | Some e -> Hashtbl.add_exn live_ranges ~key:x ~data:(i, e)
                    | None -> () (* must have been a define that was never used *));
                (* add uses if they don't already exist *)
                List.iter uses ~f:(fun x -> Hashtbl.add active ~key:x ~data:(i - 1) |> ignore);
                go (i - 2) stmts in
    let res = go ((n - 1) * 2) rev_asm in
    let print_lvalue = function
        | Reg r -> string_of_reg r
        | Temp t -> Printf.sprintf "%%t%d" (Temp.number t) in
    Hashtbl.to_alist res
    |> List.iter ~f:(fun (a, (b, c)) -> Printf.printf "%s: %d, %d\n" (print_lvalue a) b c);
    res

let linear_scan (live_ranges : (LValueTemp.t, int * int) Hashtbl.t) =
    let live_ranges_sort_start =
        Hashtbl.to_alist live_ranges
        |> List.map ~f:(fun (x, (y, z)) -> (x, y, z))
        |> List.sort ~compare:(fun (_, s1, _) (_, s2, _) -> Int.compare s1 s2) in
    let register_mapping = Hashtbl.create (module Temp) in
    (* TODO: better way of handling free registers, spilling? *)
    let free_regs = ref all_regs in
    let fresh_reg () = match !free_regs with
        | [] -> failwith "ran out of registers"
        | reg :: regs -> free_regs := regs; reg in
    let use_phys_reg reg =
        let free_regs' = List.filter !free_regs ~f:(fun x -> x <> reg) in
        if List.length !free_regs = List.length free_regs' then 
            failwith "was required to use same physical register simultaneously";
        free_regs := free_regs' in
    let free_reg reg =
        if List.mem !free_regs reg ~equal:(=) then failwith "reg was already free";
        let () = Printf.printf "freeing %s\n" (string_of_reg reg) in
        free_regs := reg :: !free_regs in
    let expire start active =
        let (expired, active) = List.partition_tf active 
            ~f:(fun (_, _, stop') -> stop' < start) in
        List.iter expired ~f:(fun (t, _, _) -> 
            let () = print_endline "WOOOO" in
            let reg = match t with
            | Reg r -> r
            | Temp t -> Hashtbl.find_exn register_mapping t in
            free_reg reg);
        active in
    let rec go active = function
        | [] -> register_mapping
        | range :: ranges ->
                let (lvalue, start, _) = range in
                let active = expire start active in
                (match lvalue with
                | Reg r -> use_phys_reg r
                | Temp t -> Hashtbl.add_exn register_mapping ~key:t ~data:(fresh_reg ()));
                go (range :: active) ranges
    in
    go [] live_ranges_sort_start

let register_allocate asm =
    let ranges = compute_live_ranges asm in
    let reg_assignment = linear_scan ranges in
    let map_lvalue = function
        | Reg r -> Reg r
        | Temp t -> Temp (Hashtbl.find_exn reg_assignment t) in
    let map_operand = function
        | Imm i -> Imm i
        | LValue l -> LValue (map_lvalue l) in
    let map_stmt = function
        | TwoAddr (two_op, o, l) -> TwoAddr (two_op, map_operand o, map_lvalue l)
        | Mov (o, l) -> Mov(map_operand o, map_lvalue l) in
    List.map ~f:map_stmt asm
