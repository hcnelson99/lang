open Core
open Ir

let prologue stack_size = 
Printf.sprintf {|
.global my_main
my_main:
pushq %%rbp
pushq %%rbx
pushq %%r12
pushq %%r13
pushq %%r14
pushq %%r15
movq %%rsp, %%rbp
subq $%d, %%rsp
|} stack_size

let epilogue stack_size = 
Printf.sprintf {|
addq $%d, %%rsp
popq %%r15
popq %%r14
popq %%r13
popq %%r12
popq %%rbx
popq %%rbp
retq
|} stack_size

(* functions to emit ir directly *)
let emit_temp t = Printf.sprintf "-%d(%%rbp)" (8 * Temp.number t)
let emit_operand = function
    | IntVal i -> "$" ^ Int.to_string i
    | Temp t -> emit_temp t
let emit_op = function
    | Lexer.Plus -> "addq"
    | Lexer.Times -> "imulq"
    | Lexer.Divide -> failwith "not yet implemented?"
    | Lexer.Minus -> "subq"
let _emit_stmt = function
    | Return op -> "movq " ^ emit_operand op ^ ", %rax"
    | Assign (t1, op) -> 
            String.concat ~sep:"\n"
            [Printf.sprintf "movq %s, %%rax" (emit_operand op);
             Printf.sprintf "movq %%rax, %s" (emit_temp t1);
            ]
    | BinOp (res, op, lhs, rhs) ->
            String.concat ~sep:"\n"
            [Printf.sprintf "movq %s, %%rax" (emit_operand lhs);
             Printf.sprintf "%s %s, %%rax" (emit_op op) (emit_operand rhs);
             Printf.sprintf "movq %%rax, %s" (emit_temp res);
            ]

let emit (ir : Ir.program) =
    let asm = X86.lower_to_two_address ir in
    let asm = X86.register_allocate asm in
    let asm = X86.peephole asm in
    let stack_size = 0 in
    let out = prologue stack_size ^
        X86.string_of_program asm
        ^ epilogue stack_size in
    print_endline out;
    out

