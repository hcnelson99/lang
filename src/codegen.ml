open Core

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

let emit (ir : Ir.program) =
    let asm = X86.lower_to_two_address ir in
    let asm = X86.register_allocate asm in
    let asm = X86.peephole asm in
    let stack_size = X86.StackSlot.max_slot () in
    let out = prologue stack_size ^
        X86.string_of_program asm
        ^ epilogue stack_size in
    print_endline out;
    out

