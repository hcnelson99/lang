type reg = RAX | RBX | RCX | RDX | RSI | RDI | R9 | R10 | R11 | R12

type 'a operand =
    | Imm of int
    | LValue of 'a

type two_op = Imul | Add | Sub

(* 'a is the lvalue *)
type 'a stmt =
    | TwoAddr of two_op * 'a operand * 'a
    | Mov of 'a operand * 'a

type 'a program = 'a stmt list

