open Core

type reg = 
    | RAX | RBX | RCX | RDX | RSI | RDI | R8 
    | R9 | R10 | R11 | R12 | R13 | R14 | R15 [@@deriving sexp, compare, hash, equal]

let _all_regs = [RAX; RBX; RCX; RDX; RSI; RDI; R8; R9; R10; R11; R12; R13; R14; R15]
let allocatable_regs = [RAX; RBX; RCX; RDX; RSI; RDI; R8; R9; R10; R11; R12; R13; R14]
let spill_reg = R15

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
type cond = Greater | Less | Equal

type 'a stmt =
    | TwoAddr of two_op * 'a operand * 'a lvalue
    | Mov of 'a operand * 'a lvalue
    | Cmp of 'a operand * 'a lvalue
    | Setcc of cond * 'a lvalue
    | Idiv of 'a operand
    | Cqto

type 'a program = 'a stmt list

module type BAG = sig
    type 'a t
    val create : 'a list -> equal:('a -> 'a -> bool) -> 'a t
    val grab_elt : 'a t -> 'a option -> 'a option
    (* val has_elt : 'a t -> 'a -> bool *)
    val take_elt : 'a t -> 'a -> bool
    (* val take_elt_exn : 'a t -> 'a -> unit *)
    val add_elt : 'a t -> 'a -> unit
    val transfer_all : 'a t -> src:'a t -> unit
end

module Bag : BAG = struct
    (* TODO: O(n) datastructure. switch to hash set? *)
    type 'a t = {
            mutable elts : 'a list;
            equal : 'a -> 'a -> bool
        }
    let create elts ~equal = {
            elts;
            equal
        }

    let has_elt {elts; equal} elt = List.mem elts elt ~equal

    let grab_elt ({elts; equal} as bag) preference = match elts with
        | []  -> None
        | x::xs ->  match preference with
            | None -> (bag.elts <- xs; Some x)
            | Some pref -> if has_elt bag pref
                then (bag.elts <- List.filter ~f:(fun x -> not (equal x pref)) elts; Some pref)
                else (bag.elts <- xs; Some x)

    let take_elt ({elts; equal} as bag) elt = 
        let elts' = List.filter elts ~f:(fun x -> not (equal x elt)) in
        if List.length elts' = List.length elts
        then false
        else (bag.elts <- elts'; true)

    let add_elt bag elt = 
        assert (not (has_elt bag elt));
        bag.elts <- elt :: bag.elts

    let transfer_all bag ~src = 
        bag.elts <- src.elts @ bag.elts;
        src.elts <- []
       
end

module type STACKSLOT = sig
    type t [@@deriving sexp, compare, hash, equal]
    val fresh : t option -> t
    val retire : t -> unit

    val offset : t -> int

    val max_slot : unit -> int
end

module StackSlot : STACKSLOT = struct
    type t = int [@@deriving sexp, compare, hash, equal]

    let next = ref 1
    let free = Bag.create [] ~equal:Int.equal

    let fresh preference = match Bag.grab_elt free preference with
        | None -> let res = !next in (next := res + 1; res)
        | Some s ->  s

    let offset s = s * 8
    let retire slot = Bag.add_elt free slot

    (* TODO: could be better. two-pass solution? *)
    let max_slot () = !next * 8
end

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

let string_of_temp_lvalue = function
    | Reg r -> string_of_reg r
    | Temp s -> Temp.string_of_t s

let string_of_stackslot_lvalue = function
    | Reg r -> string_of_reg r
    | Temp s -> Printf.sprintf "-%d(%%rbp)" (StackSlot.offset s)
    
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
    | Cmp (op, lvalue) -> "cmpq " ^ string_of_operand sol op ^ ", " ^ sol lvalue
    | Setcc (cond, lvalue) -> 
            let cc = match cond with
            | Greater -> "g" in
            "set" ^ cc ^ " " ^ sol lvalue
    | Idiv (op) -> "idivq " ^ string_of_operand sol op
    | Cqto -> "cqto"

let string_of_program program =
    program
    |> List.map ~f:(string_of_stmt string_of_stackslot_lvalue)
    |> String.concat ~sep:"\n"

let lower_to_two_address ir =
    let lower_operand = function
        | Ir.IntVal i -> Imm i
        | Ir.Temp t -> LValue (Temp t) in
    let lower_standard_binop = function
        | Lexer.Plus -> Add
        | Lexer.Times -> Imul
        | Lexer.Minus -> Sub
        | _ -> failwith "ICE: not a standard binop" in
    let lower_stmt = function
        | Ir.Return op -> [Mov (lower_operand op, Reg RAX)]
        | Ir.Assign (t, op) -> [Mov (lower_operand op, Temp t)]
        | Ir.BinOp (t, binop, op1, op2) -> match binop with
            | Lexer.Greater -> [
                Mov (lower_operand op1, Temp t);
                Cmp (lower_operand op2, Temp t);
                Mov (Imm 0, Temp t);
                Setcc (Greater, Temp t);
                ]
            | Lexer.Divide -> [
                Mov (lower_operand op1, Reg RAX);
                Cqto;
                Idiv (lower_operand op2);
                Mov (LValue (Reg RAX), Temp t)
                ]
            | Lexer.Plus | Lexer.Times | Lexer.Minus ->
                (* TODO use commutativity to reduce copies *)
                let op = lower_standard_binop binop in
                [Mov (lower_operand op1, Temp t);
                 TwoAddr (op, lower_operand op2, Temp t)] in
    List.concat_map ~f:lower_stmt ir

module StackSlotLValue = struct
    type t = StackSlot.t lvalue [@@deriving sexp, compare, hash, equal]
end

module TempLValue = struct
    type t = Temp.t lvalue [@@deriving sexp, compare, hash, equal]
end


(* return (stmt * creations * retired) list *)
let compute_live_ranges asm : (Temp.t stmt * TempLValue.t list * TempLValue.t list) list =
    let active = Hash_set.create (module TempLValue) in
    let rev_asm = List.rev asm in
    let operand_to_lvalue = function
        | Imm _ -> []
        | LValue x -> [x] in
    let get_defines = function
        | TwoAddr(_, _, lvalue) -> [lvalue]
        | Mov (_, lvalue) -> [lvalue]
        | Cmp _ -> []
        | Setcc (_, lvalue) -> [lvalue]
        | Idiv _ -> [Reg RAX; Reg RDX]
        | Cqto -> [Reg RAX; Reg RDX] in
    let get_uses = function
        | TwoAddr(_, op, lvalue) -> lvalue :: operand_to_lvalue op 
        | Mov (op, _) -> operand_to_lvalue op
        | Cmp (op, lvalue) -> lvalue :: operand_to_lvalue op
        (* also uses lvalue since the high bits are kept *)
        | Setcc (_, lvalue) -> [lvalue]
        | Idiv op -> operand_to_lvalue op @ [Reg RAX; Reg RDX] 
        | Cqto -> [Reg RAX] in
    let rec go acc = function
        | [] -> acc
        | stmt :: stmts -> 
                (* TODO: this will let the same register retire multiple times
                 * unfortunately. Basically, if a register is re-defined, it
                 * can be created, expire, be created again, and expire again
                 * (get live-range split). I actually think this is okay since
                 * our register allocator below already deals with this
                 * properly (because it does everything in one pass) *)
                let (defines, uses) = (get_defines stmt, get_uses stmt) in
                (* things that were defined on this line but not used are creations *)
                let creations = List.filter defines 
                    ~f:(fun x -> not (List.mem uses x ~equal:TempLValue.equal)) in
                List.iter creations ~f:(fun x -> 
                    Hash_set.remove active x
                );
                (* add uses if they don't already exist *)
                (* something retires here if we are adding it to the active list for the first time *)
                let retires = List.filter_map uses ~f:(fun x -> 
                    if Hash_set.mem active x then None
                    else (Hash_set.add active x; Some x)
                ) in
                go ((stmt, creations, retires) :: acc) stmts in
    let res = go [] rev_asm in
    let p_tl = List.to_string ~f:string_of_temp_lvalue in
    List.iter res ~f:(fun (stmt, creations, retires) ->
        Printf.printf "%s (created: %s, retired: %s\n" (string_of_stmt string_of_temp_lvalue stmt) (p_tl creations) (p_tl retires));
    res

let register_allocate (asm : Temp.t program) =
    let live_ranges_asm = compute_live_ranges asm in
    let register_mapping : (Temp.t, StackSlot.t lvalue) Hashtbl.t = Hashtbl.create (module Temp) in

    let free_regs = Bag.create allocatable_regs ~equal:equal_reg in

    (* For debugging: always spill *)
    (* let fresh_lvalue _ = Temp (StackSlot.fresh None) in *)

    let fresh_reg (preference : reg option) = match Bag.grab_elt free_regs preference with
            | None -> Temp (StackSlot.fresh None)
            | Some reg -> Reg reg in

    (* TODO: between fresh_lvalue and fresh_reg, all of this logic is quite
     * complicated and not great... *)
    let fresh_lvalue (preference : StackSlot.t lvalue option) = match preference with
        | None -> fresh_reg None
        | Some (Reg r) -> fresh_reg (Some r)
        | Some (Temp t) -> match Bag.grab_elt free_regs None with
            | None -> Temp (StackSlot.fresh (Some t))
            | Some r -> (Reg r) in

    let get_mapped_lvalue = function
        | Reg r -> Reg r
        | Temp t -> Hashtbl.find_exn register_mapping t in

    let rec go acc = function
        | [] -> List.rev acc
        | (stmt, creations, retires)  :: ranges ->
                let retired_regs = Bag.create ~equal:equal_reg [] in

                List.map ~f:get_mapped_lvalue retires
                |> List.iter ~f:(function
                    | Temp s -> StackSlot.retire s
                    | Reg r -> Bag.add_elt retired_regs r);

                (* List.iter retires ~f:retire; *)

                let reg_creations, temp_creations = List.partition_map ~f:(function 
                    | Reg r -> `Fst r
                    | Temp t -> `Snd t) creations in

                let evictions = List.filter_map reg_creations ~f:(fun reg ->
                    if Bag.take_elt retired_regs reg then None
                    else if Bag.take_elt free_regs reg then None
                    else let replacement = fresh_lvalue None in
                    Some (reg, replacement)
                ) in

                Bag.transfer_all free_regs ~src:retired_regs;

                List.iter temp_creations ~f:(fun t ->
                            (* For moves, try to allocate the same register as
                             * the source register (coalescing). NOP moves will
                             * be cleaned up by a later pass *)
                            let src_reg = match stmt with
                            | Mov (src, _) -> begin match src with
                                | Imm _ -> None
                                | LValue (Reg r) -> Some (Reg r)
                                | LValue (Temp t) -> Some (Hashtbl.find_exn register_mapping t)
                                end
                            | _ -> None in
                            Hashtbl.add_exn register_mapping ~key:t ~data:(fresh_lvalue src_reg));

                let map_lvalue = function
                    | Reg r -> Reg r
                    | Temp t -> Hashtbl.find_exn register_mapping t in
                let map_operand = function
                    | Imm i -> Imm i
                    | LValue l -> LValue (map_lvalue l) in
                let map_stmt = function
                    | TwoAddr (two_op, o, l) -> TwoAddr (two_op, map_operand o, map_lvalue l)
                    | Mov (o, l) -> Mov(map_operand o, map_lvalue l)
                    | Cmp (o, l) -> Cmp(map_operand o, map_lvalue l)
                    | Setcc (cond, l) -> Setcc(cond, map_lvalue l)
                    | Idiv o -> Idiv (map_operand o)
                    | Cqto -> Cqto in

                let stmt' = map_stmt stmt in
                (* after mapping the statement, change the register mapping
                 * so that things that were evicted will be referred to in
                 * their new locations afterwards *)
                let extra_moves = List.map ~f:(fun (reg, eviction) ->
                    (* TODO: this is a little slow? *)
                    (Hashtbl.map_inplace ~f:(fun x -> match x with 
                        | Reg r -> if equal_reg reg r then eviction else x
                        | Temp _ -> x) register_mapping;
                    Mov (LValue (Reg reg), eviction))) evictions in
                go (stmt' :: extra_moves @ acc) ranges
    in

    let reg_allocated_asm = go [] live_ranges_asm in

    let remove_double_memory_references asm = 
        let modify_stmt stmt = match stmt with
            | Mov (LValue (Temp src), Temp dst) ->
                    [ Mov (LValue (Temp src), Reg spill_reg);
                      Mov (LValue (Reg spill_reg), Temp dst) ]
            (* For imuls, the destination cannot be a memory address *)
           | TwoAddr (Imul, LValue (Temp src), Temp dst) ->
                    [ Mov (LValue (Temp dst), Reg spill_reg);
                      TwoAddr (Imul, LValue (Temp src), Reg spill_reg);
                      Mov (LValue (Reg spill_reg), Temp dst);
                    ]
            | TwoAddr (two_op, LValue (Temp src), Temp dst) ->
                    [ Mov (LValue (Temp src), Reg spill_reg);
                      TwoAddr (two_op, LValue (Reg spill_reg), Temp dst)
                    ]
            | _ -> [stmt]
            in
        List.concat_map ~f:modify_stmt asm in

    remove_double_memory_references reg_allocated_asm


let peephole_stmt stmt = match stmt with
    | Mov (LValue (Reg src), Reg dst) -> if equal_reg src dst then [] else [stmt]
    | Mov (LValue (Temp src), Temp dst) -> if StackSlot.equal src dst then [] else [stmt]
    | _ -> [stmt]

let peephole = List.concat_map ~f:peephole_stmt
