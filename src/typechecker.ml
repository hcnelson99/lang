open Core

type bound =
  | Let_bound
  | Fun_bound

type context = (Hir.Var.t * bound * Ty.t) Symbol.Map.t

exception TypeError of string

let rec is_syntactic_value e =
  match Mark.obj e with
  | Ast.Var _ | Ast.Bool _ | Ast.Int _ | Ast.Abs _ -> true
  | Ast.Tuple ts -> List.fold ~init:true ~f:(fun b x -> b && is_syntactic_value x) ts
  (* Is splitting a syntactic value? Probably not but idk *)
  | Ast.Split _ | Ast.Ap _ | Ast.Let _ -> false
;;

let check_sym_list_for_dups xs =
  let table = Symbol.Table.create () in
  xs
  |> List.map ~f:Mark.obj
  |> List.iter ~f:(fun x ->
         match Hashtbl.add table ~key:x ~data:() with
         | `Duplicate -> raise (TypeError "dup in sym list")
         | `Ok -> ())
;;

let rec infer (ctx : context) (e : Ast.mexp) =
  match Mark.obj e with
  | Var x ->
    (match Symbol.Map.find ctx x with
    | None -> raise (TypeError "var not in ctx")
    | Some (v, Let_bound, ty) -> Ty.Union_find.instantiate ty, Hir.Var v
    | Some (v, Fun_bound, ty) -> ty, Hir.Var v)
  | Int i -> Ty.constructor Ty.Constructor.Int [], Hir.Int i
  | Bool b -> Ty.constructor Ty.Constructor.Bool [], Hir.Bool b
  | Tuple es ->
    let es = List.map ~f:(infer ctx) es in
    let ts = List.map ~f:fst es in
    Ty.constructor Ty.Constructor.Tuple ts, Hir.Tuple es
  | Split (e1, xs, e2) ->
    check_sym_list_for_dups xs;
    let syms_and_types = List.map xs ~f:(fun x -> Mark.obj x, Ty.unconstrained ()) in
    let tuple_ty =
      Ty.constructor Ty.Constructor.Tuple (syms_and_types |> List.map ~f:snd)
    in
    let ((tau1, _) as h_e1) = infer ctx e1 in
    Ty.Union_find.unify tau1 tuple_ty;
    let ctx', vs =
      List.fold_map
        ~init:ctx
        ~f:(fun ctx (x, ty) ->
          let v = Hir.Var.create (Symbol.name x) in
          Symbol.Map.set ctx ~key:x ~data:(v, Fun_bound, ty), v)
        syms_and_types
    in
    let ((tau2, _) as h_e2) = infer ctx' e2 in
    tau2, Hir.Split (h_e1, vs, h_e2)
  | Abs (x, e) ->
    let tau = Ty.unconstrained () in
    let x = Mark.obj x in
    let v = Hir.Var.create (Symbol.name x) in
    let ctx' = Symbol.Map.set ctx ~key:x ~data:(v, Fun_bound, tau) in
    let ((tau', _) as h_e) = infer ctx' e in
    Ty.constructor Ty.Constructor.Arrow [ tau; tau' ], Hir.Abs (v, h_e)
  | Let (x, e0, e1) ->
    let ((tau, _) as h_e0) = infer ctx e0 in
    let x = Mark.obj x in
    (* OPT: mark non-polymorphic let-bound types as fun_bound (since they don't need to be instantiated *)
    let binding_type = if is_syntactic_value e0 then Let_bound else Fun_bound in
    let v = Hir.Var.create (Symbol.name x) in
    let ctx' = Symbol.Map.set ctx ~key:x ~data:(v, binding_type, tau) in
    let ((ty, _) as h_e1) = infer ctx' e1 in
    ty, Hir.Let (v, h_e0, h_e1)
  | Ap (e0, e1) ->
    let ((tau0, _) as h_e0) = infer ctx e0 in
    let ((tau1, _) as h_e1) = infer ctx e1 in
    let tau' = Ty.unconstrained () in
    Ty.Union_find.unify tau0 (Ty.constructor Ty.Constructor.Arrow [ tau1; tau' ]);
    tau', Hir.Ap (h_e0, h_e1)
;;

let typecheck ast =
  let ctx = Symbol.Map.empty in
  let ((ty, _) as hir) = infer ctx ast in
  print_endline (Ty.to_string ty);
  (* if Ty.is_poly ty *)
  (* then raise (TypeError "Program must not be polymorphic at the top level") *)
  (* else  *)
  Hir.map_ty ~f:Ty.to_hir_ty hir
;;
