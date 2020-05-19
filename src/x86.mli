type reg = 
    | RAX | RBX | RCX | RDX | RSI | RDI | R8 
    | R9 | R10 | R11 | R12 | R13 | R14 | R15

type 'a lvalue = Reg of reg | Temp of 'a

type 'a operand =
    | Imm of int
    | LValue of 'a lvalue

type two_op = Imul | Add | Sub

type 'a stmt =
    | TwoAddr of two_op * 'a operand * 'a lvalue
    | Mov of 'a operand * 'a lvalue
    | Idiv of 'a operand
    | Cqto

type 'a program = 'a stmt list

module StackSlot : sig
    type t
    val max_slot : unit -> int
end

val string_of_program : StackSlot.t program -> string
val lower_to_two_address : Ir.program -> Temp.t program
val register_allocate : Temp.t program -> StackSlot.t program
val peephole : StackSlot.t program -> StackSlot.t program
