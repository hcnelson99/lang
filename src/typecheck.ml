open Core


let typecheck program =
    let open Ast in
    let vars = Hashtbl.create (module Symbol) in
    let add_defn lhs = Hashtbl.add ~key:lhs ~data:() vars |> ignore in
    let check_defn s = match Hashtbl.find vars s with
        | None -> failwith "variable not defined"
        | Some _ -> () in
    let rec typecheck_exp = function
        | Variable s -> check_defn s
        | Operator (_, e1, e2) -> typecheck_exp e1; typecheck_exp e2
        | IntVal _ -> () in
    let rec go = function
        | [] -> ()
        | stmt :: stmts -> match stmt with
            | Assign (l, e) ->
                    typecheck_exp e;
                    add_defn l;
                    go stmts
            | Return e ->
                    typecheck_exp e;
                    if stmts <> [] then failwith "statements after return"
                    else () in
    go program
