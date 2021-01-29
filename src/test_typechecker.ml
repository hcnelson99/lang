open Core

let typecheck_test program =
  let lexbuf = Lexing.from_string program in
  let ast = Parser.program Lexer.initial lexbuf in
  try
    ast
    |> Typechecker.typecheck
    |> List.iter ~f:(fun stmt ->
           match stmt with
           | LetStmt (v, (ty, _)) ->
             print_endline (Hir.Var.to_string_hum v ^ " : " ^ Hir.Ty.to_string_hum ty))
  with
  | Compile_error.Error s -> print_endline ("Error: " ^ s)
  | Parser.Error ->
    print_endline (Error_msg.error ~msg:"Parse error" lexbuf |> Error_msg.error_to_string)
;;

let%expect_test _ =
  typecheck_test {|
  let id = fun x -> x
  end
  |};
  [%expect {| id : 'a -> 'a |}]
;;

let%expect_test "generalization" =
  typecheck_test {|
  let f = fun x -> x
  end
  let v = (f 1, f true)
  end
  |};
  [%expect {|
    f : 'a -> 'a
    v : Int * Bool |}]
;;

let%expect_test "value restriction" =
  typecheck_test
    {|
  let exp = 
    let id = fun x -> x in
    let f = id id in
    (f 1, f true)
  end
  |};
  [%expect {| Error: unification error |}]
;;

let%expect_test _ =
  typecheck_test
    {|
  let exp =
    let id = fun x -> x in
    let f = id id in
    f 1
  end
  |};
  [%expect {| exp : Int |}]
;;

let%expect_test _ =
  typecheck_test {|
  let exp = 
    (fun x -> x, fun x -> x)
  end
  |};
  [%expect {| exp : ('a -> 'a) * ('b -> 'b) |}]
;;

let%expect_test _ =
  typecheck_test {|
  let f =
    fun x -> split x as (x, y) in (y, x)
  end
  |};
  [%expect {| f : ('b * 'a) -> ('a * 'b) |}]
;;

let%expect_test _ =
  typecheck_test
    {|
  let exp =
    let f = fun x ->
      let g = fun y -> (y, x) in
      g 1 in
    f 1
  end
  |};
  [%expect {| exp : Int * Int |}]
;;

let%expect_test _ =
  typecheck_test
    {|
  let f = fun x ->
    let g = fun y -> (y, x) in
    (g 1, g true)
  end
  |};
  [%expect {| f : 'a -> ((Int * 'a) * (Bool * 'a)) |}]
;;

let%expect_test _ =
  typecheck_test
    {|
  let f = fun x -> (x, x)
  end
  let g = fun y -> (f y, f 3)
  end
  let exp = (g true, g 1)
  end
  |};
  [%expect
    {|
    f : 'a -> ('a * 'a)
    g : 'a -> (('a * 'a) * (Int * Int))
    exp : ((Bool * Bool) * (Int * Int)) * ((Int * Int) * (Int * Int)) |}]
;;
