open Core

let main () = 
    let () = print_endline (Sys.getcwd ()) in
    match Array.to_list (Sys.get_argv ()) with
    | [_; fname] -> (Toplevel.compile_and_exec fname : int option) |> ignore
    | _ -> failwith "Incorrect arguments"

