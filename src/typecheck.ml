open Core
open Ast

type init = Uninitialized | Initialized
type ty = Bool | Int | Alpha [@@deriving equal]

let show_ty = function
    | Bool -> "bool"
    | Int -> "int"
    | Alpha -> "'a"

let typecheck lexer (program : Ast.program) =
    let vars = Hashtbl.create (module Symbol) in
    let add_defn lhs = 
        match Hashtbl.add ~key:(Mark.obj lhs) ~data:(Uninitialized, Alpha) vars with
        | `Duplicate -> Lexer.error lexer lhs "redefined variable"
        | `Ok -> () in

    let expect_type e_ty ty mark =
            if equal_ty e_ty ty then () else 
            let msg = Printf.sprintf "expected type %s but had type %s" (show_ty e_ty) (show_ty ty) in
            Lexer.error lexer mark msg in

    let expect_eq_able_types ty1 ty2 mark =
            if equal_ty ty1 ty2 then () else 
            let msg = Printf.sprintf "lhs and rhs have different types in equality expression: %s and %s" (show_ty ty1) (show_ty ty2) in
            Lexer.error lexer mark msg in

    let check_use lhs = 
        match Hashtbl.find vars (Mark.obj lhs) with
            | None -> Lexer.error lexer lhs "undefined variable"
            | Some (Uninitialized, _) -> Lexer.error lexer lhs "variable defined but not initialized" 
            | Some (Initialized, ty) -> ty in

    let rec typecheck_exp exp = match Mark.obj exp with
        | Variable s -> check_use (Mark.with_mark exp s)
        | BinOp (binop, e1, e2) -> 
                let open Lexer in
                let ret_ty, op_ty = match binop with
                    | Plus | Times | Divide | Minus -> (Int, Some Int)
                    | Greater | Greater_eq | Less | Less_eq -> (Bool, Some Int)
                    | Boolean_and | Boolean_or -> (Bool, Some Bool )
                    | Equal | Not_eq -> (Bool, None) in
                let ty1 = typecheck_exp e1 in
                let ty2 = typecheck_exp e2 in
                begin match op_ty with
                | Some op_ty -> 
                        expect_type op_ty ty1 e1;
                        expect_type op_ty ty2 e2;
                | None -> 
                        expect_eq_able_types ty1 ty2 exp
                end;
                ret_ty
        | IntVal _ -> Int
        | BoolVal _ -> Bool in

    let rec go = function
        | [] -> ()
        | stmt :: stmts -> match Mark.obj stmt with
            | Declare l ->
                    add_defn l;
                    go stmts
            | Assign (lhs, e) ->
                    let rhs_ty = typecheck_exp e in
                    begin match Hashtbl.find vars (Mark.obj lhs) with
                    | None -> Lexer.error lexer lhs "undefined variable. missing `var`?"
                    | Some (_, lhs_ty) ->
                            (* TODO: handle unification in a nicer way? *)
                            let new_lhs_ty = match lhs_ty with
                            | Alpha -> rhs_ty
                            | lhs_ty ->
                                    (expect_type lhs_ty rhs_ty e;
                                    lhs_ty) in
                            Hashtbl.set vars ~key:(Mark.obj lhs) ~data:(Initialized, new_lhs_ty) 
                    end;
                    go stmts
            | Return e ->
                    let _ : ty = typecheck_exp e in
                    (* for now, allow any return type *)
                    (* expect_type Int ret_ty e; *) 
                    match stmts with
                    | [] -> ()
                    | _ -> Lexer.error lexer stmt "statements after return" in
    go program
