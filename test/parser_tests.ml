open OUnit2
open Saumon

let expression_is_number inner =
  assert_equal
    (Parser.parse [Token.of_token_kind ~kind:(Token_kind.Number inner)])
    (Ast.Literal (Ast.Number inner))

let expression_is_string inner =
  assert_equal
    (Parser.parse [Token.of_token_kind ~kind:(Token_kind.String inner)])
    (Ast.Literal (Ast.String inner))

let expression_is_true_bool () =
  assert_equal
    (Parser.parse [Token.of_token_kind ~kind:Token_kind.True])
    (Ast.Literal (Ast.Bool true))

let expression_is_false_bool () =
  assert_equal
    (Parser.parse [Token.of_token_kind ~kind:Token_kind.False])
    (Ast.Literal (Ast.Bool false))

let expression_is_nil () =
  assert_equal
    (Parser.parse [Token.of_token_kind ~kind:Token_kind.Nil])
    (Ast.Literal Ast.Nil)

let primary_tests =
  [ ("Expression is number" >:: fun _ -> expression_is_number 42.)
  ; ("Expression is string" >:: fun _ -> expression_is_string "hello")
  ; ("Expression is true bool" >:: fun _ -> expression_is_true_bool ())
  ; ("Expression is false bool" >:: fun _ -> expression_is_false_bool ())
  ; ("Expression is nil" >:: fun _ -> expression_is_nil ()) ]

let expression_is_bang_unary () =
  let bang = Token.of_token_kind ~kind:Token_kind.Bang in
  assert_equal
    (Parser.parse [bang; Token.of_token_kind ~kind:Token_kind.True])
    (Ast.Unary (bang, Ast.Literal (Ast.Bool true)))

let expression_is_minus_number n =
  let minus = Token.of_token_kind ~kind:Token_kind.Minus in
  assert_equal
    (Parser.parse [minus; Token.of_token_kind ~kind:(Token_kind.Number n)])
    (Ast.Unary (minus, Ast.Literal (Ast.Number n)))

let unary_tests =
  [ ("Expression is bang unary" >:: fun _ -> expression_is_bang_unary ())
  ; ("Expression is minus number" >:: fun _ -> expression_is_minus_number 42.)
  ]

let multiplication_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Star in
  assert_equal
    (Parser.parse
       [ Token.of_token_kind ~kind:(Token_kind.Number n)
       ; infix
       ; Token.of_token_kind ~kind:(Token_kind.Number n) ])
    (Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let division_of_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Slash in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)) in
  assert_equal actual expect

let multiplication_tests =
  [ ( "Multiplication of two numbers"
    >:: fun _ -> multiplication_of_two_numbers 42. )
  ; ("Division of three numbers" >:: fun _ -> division_of_three_numbers ()) ]

let addition_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Plus in
  assert_equal
    (Parser.parse
       [ Token.of_token_kind ~kind:(Token_kind.Number n)
       ; infix
       ; Token.of_token_kind ~kind:(Token_kind.Number n) ])
    (Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let substract_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Minus in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)) in
  assert_equal actual expect

let addition_tests =
  [ ("Addition of two numbers" >:: fun _ -> addition_of_two_numbers 42.)
  ; ("Substract three numbers" >:: fun _ -> substract_three_numbers ()) ]

let comparison_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Greater in
  assert_equal
    (Parser.parse
       [ Token.of_token_kind ~kind:(Token_kind.Number n)
       ; infix
       ; Token.of_token_kind ~kind:(Token_kind.Number n) ])
    (Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let comparison_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Less_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)) in
  assert_equal actual expect

let comparison_tests =
  [ ("Comparison of two numbers" >:: fun _ -> comparison_of_two_numbers 42.)
  ; ("Comparison three numbers" >:: fun _ -> comparison_three_numbers ()) ]

let equality_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Equal_equal in
  assert_equal
    (Parser.parse
       [ Token.of_token_kind ~kind:(Token_kind.Number n)
       ; infix
       ; Token.of_token_kind ~kind:(Token_kind.Number n) ])
    (Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let equality_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Bang_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)) in
  assert_equal actual expect

let equality_tests =
  [ ("Equality of two numbers" >:: fun _ -> equality_of_two_numbers 42.)
  ; ("Equality three numbers" >:: fun _ -> equality_three_numbers ()) ]

let () =
  run_test_tt_main ("Primary test" >::: primary_tests) ;
  run_test_tt_main ("Unary test" >::: unary_tests) ;
  run_test_tt_main ("Multiplication tests" >::: multiplication_tests) ;
  run_test_tt_main ("Addition tests" >::: addition_tests) ;
  run_test_tt_main ("Comparison tests" >::: comparison_tests) ;
  run_test_tt_main ("Equality tests" >::: equality_tests)
