open Core
include Ast_type

type context = Poly_ty.t Symbol.Map.t

exception TypeError of string

let inst alpha mty =
  let alpha_inst = TyVar.Set.to_map alpha ~f:(fun _ -> TyVar.create ()) in
  Mono_ty.map
    ~f:(fun ty ->
      match TyVar.Map.find alpha_inst ty with
      | None -> ty
      | Some nty -> nty)
    mty
;;

let free_ctx (ctx : context) =
  ctx |> Map.data |> List.map ~f:Poly_ty.free |> TyVar.Set.union_list
;;

module Union_find : sig
  type t

  module Ty : sig
    type t

    val unconstrained : unit -> t
  end

  module View : sig
    type t =
      | Var of Ty.t
      | Int
      | Arrow of Ty.t * Ty.t

    val of_ty : Ty.t -> t
  end

  val unify : t -> Ty.t -> Ty.t -> unit
  val find : t -> Ty.t -> Mono_ty.t
end = struct
  module Ty = struct
    include Var.MkVar ()

    let unconstrained = create
  end

  type t = Mono_ty.t option Ty.Table.t

  let create () = TyVar.Table.create ()

  (* TODO: do path compression *)
  (* Right now find is expensive and recursive. We should do the ocaml in-place
   * thing so that unification updates all the types in place (aka the path
   * compression) *)
  let rec find t = function
    | Mono_ty.Int -> Mono_ty.Int
    | Mono_ty.Arrow (t1, t2) -> Mono_ty.Arrow (find t t1, find t t2)
    | Mono_ty.Var ty ->
      (match Hashtbl.find t ty with
      | None ->
        Hashtbl.add_exn t ~key:ty ~data:None;
        Mono_ty.Var ty
      | Some None -> Mono_ty.Var ty
      | Some (Some child) -> find t child)
  ;;

  let rec unify uf t0 t1 =
    let t0 = find uf t0 in
    let t1 = find uf t1 in
    match t0, t1 with
    | Int, Int -> ()
    | Arrow (p1, p2), Arrow (q1, q2) ->
      unify uf p1 q1;
      unify uf p2 q2
    | Var v0, Var v1 -> Hashtbl.set uf ~key:t0 ~data:(Some (Mono_ty.Var v1))
    | Var v, ty | ty, Var v -> Hashtbl.set uf ~key:v ~data:(Some ty)
    | _, _ -> raise (TypeError "type mismatch")
  ;;
end

(* TODO: generalize should be able to be computed in constant time if we
 * "maintain the binding of type variables in the context". This supposedly can
 * also help with the occurs check in the case where unification leads to
 * circularity (like fun x -> x x) *)
(* TODO: check for circularity *)
let generalize uf ctx ty =
  (* You have to UF before you generalize to make sure you're finding all the
   * free variables correctly... the path compression / automatically mutate
   * all the other types when you unify change would be really nice here *)
  let ty = Union_find.find uf ty in
  let alpha = Set.diff (Mono_ty.free ty) (free_ctx ctx) in
  if Set.is_empty alpha then Poly_ty.Mono ty else Poly_ty.Forall (alpha, ty)
;;

let rec infer (uf : Union_find.t) (ctx : context) (e : Ast.mexp) : Hir.tyexp =
  match Mark.obj e with
  | Int i -> Mono_ty.Int, Hir.Int i
  | Var x ->
    (match Symbol.Map.find ctx x with
    | None -> raise (TypeError "var not in ctx")
    | Some (Mono ty) -> ty, Hir.Var x
    | Some (Forall (alpha, mty)) -> inst alpha mty, Hir.Var x)
  | Abs (x, e) ->
    let tau = Mono_ty.Var (TyVar.create ()) in
    let x = Mark.obj x in
    (* TODO: implement alpha-equivalence *)
    let ctx' = Symbol.Map.add_exn ctx ~key:x ~data:(Mono tau) in
    let ((tau', _) as h_e) = infer uf ctx' e in
    Mono_ty.Arrow (tau, tau'), Hir.Abs (x, h_e)
  | Let (x, e0, e1) ->
    let ((tau, _) as h_e0) = infer uf ctx e0 in
    let gen_tau = generalize uf ctx tau in
    let x = Mark.obj x in
    (* TODO: implement alpha-equivalence *)
    let ctx' = Symbol.Map.add_exn ctx ~key:x ~data:gen_tau in
    let ((ty, _) as h_e1) = infer uf ctx' e1 in
    ty, Hir.Let (x, h_e0, h_e1)
  | Ap (e0, e1) ->
    let ((tau0, _) as h_e0) = infer uf ctx e0 in
    let ((tau1, _) as h_e1) = infer uf ctx e1 in
    let tau' = TyVar.create () in
    unify uf tau0 (Mono_ty.Arrow (tau1, Mono_ty.Var tau'));
    Mono_ty.Var tau', Hir.Ap (h_e0, h_e1)
;;

let typecheck ast =
  let uf = Union_find.create () in
  let ctx = Symbol.Map.empty in
  let monoty, hexp = infer uf ctx ast in
  generalize uf ctx (Union_find.find uf monoty), (monoty, hexp)
;;
