type 'a program

module StackSlot : sig
    type t
    val max_slot : unit -> int
end

val string_of_program : StackSlot.t program -> string
val lower_to_two_address : Ir.program -> Temp.t program
val register_allocate : Temp.t program -> StackSlot.t program
val peephole : StackSlot.t program -> StackSlot.t program
