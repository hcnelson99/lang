open! Core

(* let mark_var_used_as ctx v ty = *)
(*   Symbol.Map.update ctx v ~f:(function | None ->  *)

(* let record_polymorphic_uses ctx (ty, exp) = *)
(*   match exp with *)
(*   | Var v -> mark_var_used_as ctx v ty *)
(* ;; *)

let monomorphize _ = failwith ""
