type monoty =
  | Var of Symbol.t
  | Arrow of monoty * monoty

type polyty =
  | Mono of monoty
  | Forall of Symbol.t * monoty
