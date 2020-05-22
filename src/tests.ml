(* chdir to toplevel of repo *)
let () = Sys.chdir "../../.." 

let test_success testname retval = 
    let path = "tests/" ^ testname in
    match Toplevel.compile_and_exec path with
    | Some r -> retval = r
    | None -> false

let%test _ = test_success "spill" 32
let%test _ = test_success "simple_math" 0
let%test _ = test_success "divide" 18
let%test _ = test_success "spill_divide" 52
