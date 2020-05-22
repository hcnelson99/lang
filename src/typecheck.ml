open Core
open Ast

type init = Uninitialized | Initialized

let typecheck lexer (program : Ast.program) =
    let vars = Hashtbl.create (module Symbol) in
    let add_defn lhs = 
        match Hashtbl.add ~key:(Mark.obj lhs) ~data:Uninitialized vars with
        | `Duplicate -> Lexer.error lexer lhs "redefined variable"
        | `Ok -> () in

    let check_use lhs = 
        match Hashtbl.find vars (Mark.obj lhs) with
            | None -> Lexer.error lexer lhs "undefined variable"
            | Some Uninitialized -> Lexer.error lexer lhs "variable defined but not initialized" 
            | Some Initialized -> () in

    let check_assign lhs = 
        match Hashtbl.find vars (Mark.obj lhs) with
            | None -> Lexer.error lexer lhs "undefined variable. missing `var`?"
            | Some Uninitialized -> Hashtbl.set vars ~key:(Mark.obj lhs) ~data:Initialized
            | Some Initialized -> () in

    let rec typecheck_exp exp = match Mark.obj exp with
        | Variable s -> check_use (Mark.with_mark exp s)
        | BinOp (_, e1, e2) -> typecheck_exp e1; typecheck_exp e2
        | IntVal _ -> () in
    let rec go = function
        | [] -> ()
        | stmt :: stmts -> match Mark.obj stmt with
            | Declare l ->
                    add_defn l;
                    go stmts
            | Assign (l, e) ->
                    typecheck_exp e;
                    check_assign l;
                    go stmts
            | Return e ->
                    typecheck_exp e;
                    match stmts with
                    | [] -> ()
                    | _ -> Lexer.error lexer stmt "statements after return" in
    go program
