open Core

type bound =
  | Let_bound
  | Fun_bound

type context = (bound * Ty.t) Symbol.Map.t

exception TypeError of string

let is_syntactic_value e =
  match Mark.obj e with
  | Ast.Var _ -> true
  | Ast.Int _ -> true
  | Ast.Ap _ -> false
  | Ast.Abs _ -> true
  | Ast.Let _ -> false
;;

let rec infer (ctx : context) (e : Ast.mexp) =
  match Mark.obj e with
  | Int i -> Ty.int_, Hir.Int i
  | Var x ->
    (match Symbol.Map.find ctx x with
    | None -> raise (TypeError "var not in ctx")
    | Some (Let_bound, ty) -> Ty.Union_find.instantiate ty, Hir.Var x
    | Some (Fun_bound, ty) -> ty, Hir.Var x)
  | Abs (x, e) ->
    let tau = Ty.unconstrained () in
    let x = Mark.obj x in
    (* TODO: implement alpha-equivalence *)
    let ctx' = Symbol.Map.add_exn ctx ~key:x ~data:(Fun_bound, tau) in
    let ((tau', _) as h_e) = infer ctx' e in
    Ty.arrow (tau, tau'), Hir.Abs (x, h_e)
  | Let (x, e0, e1) ->
    let ((tau, _) as h_e0) = infer ctx e0 in
    let x = Mark.obj x in
    (* TODO: implement alpha-equivalence *)
    (* OPT: mark non-polymorphic let-bound types as fun_bound (since they don't need to be instantiated *)
    let binding_type = if is_syntactic_value e0 then Let_bound else Fun_bound in
    let ctx' = Symbol.Map.add_exn ctx ~key:x ~data:(binding_type, tau) in
    let ((ty, _) as h_e1) = infer ctx' e1 in
    ty, Hir.Let (x, h_e0, h_e1)
  | Ap (e0, e1) ->
    let ((tau0, _) as h_e0) = infer ctx e0 in
    let ((tau1, _) as h_e1) = infer ctx e1 in
    let tau' = Ty.unconstrained () in
    Ty.Union_find.unify tau0 (Ty.arrow (tau1, tau'));
    tau', Hir.Ap (h_e0, h_e1)
;;

let typecheck ast =
  let ctx = Symbol.Map.empty in
  let ((ty, _) as hir) = infer ctx ast in
  if Ty.is_poly ty
  then raise (TypeError "Program must not be polymorphic at the top level")
  else Hir.map_ty ~f:Ty.to_hir_ty hir
;;
