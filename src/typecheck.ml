open Core
open Ast

let typecheck lexer (program : Ast.program) =
    let vars = Hashtbl.create (module Symbol) in
    let add_defn lhs = 
        (Hashtbl.add ~key:lhs ~data:() vars : [`Duplicate | `Ok]) 
        |> ignore in

    let check_defn lhs = 
        match Hashtbl.find vars (Mark.obj lhs) with
            | None -> Lexer.error lexer lhs "undefined variable. missing `var`?"
            | Some _ -> () in

    let rec typecheck_exp exp = match Mark.obj exp with
        | Variable s -> check_defn (Mark.with_mark exp s)
        | BinOp (_, e1, e2) -> typecheck_exp e1; typecheck_exp e2
        | IntVal _ -> () in
    let rec go = function
        | [] -> ()
        | stmt :: stmts -> match Mark.obj stmt with
            | Declare l ->
                    add_defn (Mark.obj l);
                    go stmts
            | Assign (l, e) ->
                    typecheck_exp e;
                    check_defn l;
                    go stmts
            | Return e ->
                    typecheck_exp e;
                    match stmts with
                    | [] -> ()
                    | _ -> Lexer.error lexer stmt "statements after return" in
    go program
