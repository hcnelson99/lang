let typecheck_test program =
  let lexbuf = Lexing.from_string program in
  let ast = Parser.program Lexer.initial lexbuf in
  let ty, _ = Typechecker.typecheck ast in
  print_endline (Hir.Ty.to_string_hum ty)
;;

let%expect_test _ =
  typecheck_test {|
  fun x -> x
  |};
  [%expect {| 'a -> 'a |}]
;;

let%expect_test _ =
  typecheck_test {|
  let f = fun x -> x in
  (f 1, f true)
  |};
  [%expect {| Int * Bool |}]
;;

let%expect_test _ =
  typecheck_test {|
  (fun x -> x, fun x -> x)
  |};
  [%expect {| ('a -> 'a) * ('b -> 'b) |}]
;;

let%expect_test _ =
  typecheck_test {|
  fun x -> split x with (x, y) in (y, x)
  |};
  [%expect {| ('b * 'a) -> ('a * 'b) |}]
;;
