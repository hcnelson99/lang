open Core

type bound =
  | Let_bound of int
  | Fun_bound

let rec is_syntactic_value e =
  match Mark.obj e with
  | Ast.Var _ | Ast.Bool _ | Ast.Int _ | Ast.Abs _ -> true
  | Ast.Tuple ts -> List.fold ~init:true ~f:(fun b x -> b && is_syntactic_value x) ts
  (* Is splitting a syntactic value? Probably not but idk. maybe it should be??? *)
  | Ast.Split _ | Ast.Ap _ | Ast.Let _ -> false
;;

let check_sym_list_for_dups xs =
  let table = Symbol.Table.create () in
  xs
  |> List.map ~f:Mark.obj
  |> List.iter ~f:(fun x ->
         match Hashtbl.add table ~key:x ~data:() with
         | `Duplicate -> raise (Compile_error.Error "dup in sym list")
         | `Ok -> ())
;;

type context = (Hir.Var.t * bound * Ty.t) Symbol.Map.t

let infer e =
  let rec go (ctx : context) let_depth e =
    let unconstrained () = Ty.unconstrained ~let_depth in
    match Mark.obj e with
    | Ast.Var x ->
      (match Symbol.Map.find ctx x with
      | None -> raise (Compile_error.Error "var not in ctx")
      | Some (v, Let_bound var_let_depth, ty) ->
        Ty.Union_find.instantiate ~var_let_depth ty, Hir.Var v
      | Some (v, Fun_bound, ty) -> ty, Hir.Var v)
    | Ast.Int i -> Ty.constructor Ty.Constructor.Int [], Hir.Int i
    | Ast.Bool b -> Ty.constructor Ty.Constructor.Bool [], Hir.Bool b
    | Ast.Tuple es ->
      let es = List.map ~f:(go ctx let_depth) es in
      let ts = List.map ~f:fst es in
      Ty.constructor Ty.Constructor.Tuple ts, Hir.Tuple es
    | Split (e1, xs, e2) ->
      check_sym_list_for_dups xs;
      let syms_and_types = List.map xs ~f:(fun x -> Mark.obj x, unconstrained ()) in
      let tuple_ty =
        Ty.constructor Ty.Constructor.Tuple (syms_and_types |> List.map ~f:snd)
      in
      let ((tau1, _) as h_e1) = go ctx let_depth e1 in
      Ty.Union_find.unify tau1 tuple_ty;
      let ctx', vs =
        List.fold_map
          ~init:ctx
          ~f:(fun ctx (x, ty) ->
            let v = Hir.Var.create (Symbol.name x) in
            Symbol.Map.set ctx ~key:x ~data:(v, Fun_bound, ty), v)
          syms_and_types
      in
      let ((tau2, _) as h_e2) = go ctx' let_depth e2 in
      tau2, Hir.Split (h_e1, vs, h_e2)
    | Ast.Abs (x, e) ->
      let tau = unconstrained () in
      let x = Mark.obj x in
      let v = Hir.Var.create (Symbol.name x) in
      let ctx' = Symbol.Map.set ctx ~key:x ~data:(v, Fun_bound, tau) in
      let ((tau', _) as h_e) = go ctx' let_depth e in
      Ty.constructor Ty.Constructor.Arrow [ tau; tau' ], Hir.Abs (v, h_e)
    | Ast.Let (x, e0, e1) ->
      let ((tau, _) as h_e0) = go ctx (let_depth + 1) e0 in
      let x = Mark.obj x in
      (* OPT: mark non-polymorphic let-bound types as fun_bound (since they don't need to be instantiated *)
      let binding_type =
        if is_syntactic_value e0 then Let_bound let_depth else Fun_bound
      in
      let v = Hir.Var.create (Symbol.name x) in
      let ctx' = Symbol.Map.set ctx ~key:x ~data:(v, binding_type, tau) in
      let ((ty, _) as h_e1) = go ctx' let_depth e1 in
      ty, Hir.Let (v, h_e0, h_e1)
    | Ast.Ap (e0, e1) ->
      let ((tau0, _) as h_e0) = go ctx let_depth e0 in
      let ((tau1, _) as h_e1) = go ctx let_depth e1 in
      let tau' = unconstrained () in
      Ty.Union_find.unify tau0 (Ty.constructor Ty.Constructor.Arrow [ tau1; tau' ]);
      tau', Hir.Ap (h_e0, h_e1)
  in
  go Symbol.Map.empty 0 e
;;

let typecheck ast =
  let hir = infer ast in
  Hir.map_ty ~f:Ty.to_hir_ty hir
;;
