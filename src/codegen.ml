open Core
open Ir

let prologue stack_size = 
Printf.sprintf {|
.global my_main
my_main:
pushq %%rbp
movq %%rsp, %%rbp
subq $%d, %%rsp
|} stack_size

let epilogue stack_size = 
Printf.sprintf {|
addq $%d, %%rsp
popq %%rbp
retq
|} stack_size

let emit_temp t = Printf.sprintf "-%d(%%rbp)" (8 * Temp.number t)
let emit_operand = function
    | IntVal i -> "$" ^ Int.to_string i
    | Temp t -> emit_temp t

let emit_op = function
    | Lexer.Plus -> "addq"
    | Lexer.Times -> "imulq"
    | Lexer.Divide -> failwith "not yet implemented?"
    | Lexer.Minus -> "subq"

let emit_stmt = function
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
    let () = ir
        |> X86.lower_to_two_address 
        |> X86.string_of_program
        |> print_endline in
    let () = X86.liveness (X86.lower_to_two_address ir) in
    let stack_size = 8 * Temp.max_temp_hack () in
    prologue stack_size ^
    (List.map ~f:emit_stmt ir
    |> String.concat ~sep:"\n")
    ^ epilogue stack_size
