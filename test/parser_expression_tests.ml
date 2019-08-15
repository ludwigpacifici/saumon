open Base
open Saumon
module A = Alcotest_testable

let check_parse =
  Alcotest.(check (result A.ast_program (pair (list string) (option A.token))))
    "check Parser.parse"

let check_true = Alcotest.(check bool) "check Parser.parse" true

(* Helper to add a semicolon at the end of an expression *)
let statement_of_expression e =
  let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon in
  Ast.Expression_statement (e, semicolon)

(* Helper to parse a list of token representing an expression and add a
   semicolon at the end *)
let parse_with_semicolon ts =
  let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon in
  Parser.parse (ts @ [semicolon])

(* Helper that assert a token list will be parsed as the given expression *)
let assert_parse ts expected_expression =
  check_parse (parse_with_semicolon ts)
    ( expected_expression |> statement_of_expression |> Ast.Program.of_statement
    |> Result.return )

(* Helper that assert a token list will be parsed and generate errors *)
let assert_parse_is_error ts =
  check_true (parse_with_semicolon ts |> Result.is_error)

let expression_is_number inner () =
  let expression = Ast.Literal (Ast.Number inner) in
  assert_parse [Token.of_token_kind ~kind:(Token_kind.Number inner)] expression

let expression_is_string inner () =
  let expression = Ast.Literal (Ast.String inner) in
  assert_parse [Token.of_token_kind ~kind:(Token_kind.String inner)] expression

let expression_is_true_bool () =
  let expression = Ast.Literal (Ast.Bool true) in
  assert_parse [Token.of_token_kind ~kind:Token_kind.True] expression

let expression_is_false_bool () =
  let expression = Ast.Literal (Ast.Bool false) in
  assert_parse [Token.of_token_kind ~kind:Token_kind.False] expression

let expression_is_nil () =
  let expression = Ast.Literal Ast.Nil in
  assert_parse [Token.of_token_kind ~kind:Token_kind.Nil] expression

let expression_is_identifier x () =
  let expression = Ast.Literal (Ast.Identifier x) in
  assert_parse [Token.of_token_kind ~kind:(Token_kind.Identifier x)] expression

let expression_is_grouping () =
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  let right = Token.of_token_kind ~kind:Token_kind.Right_paren in
  let expression = Ast.Grouping (left, Ast.Literal (Ast.Bool true), right) in
  assert_parse
    [left; Token.of_token_kind ~kind:Token_kind.True; right]
    expression

let expression_grouping_bad_closed_paren () =
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  assert_parse_is_error [left; Token.of_token_kind ~kind:Token_kind.True; left]

let expression_grouping_missing_closed_paren () =
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  assert_parse_is_error [left; Token.of_token_kind ~kind:Token_kind.True]

let expression_is_illegal () =
  assert_parse_is_error [Token.of_token_kind ~kind:Token_kind.Return]

let expression_is_bang_unary () =
  let bang = Token.of_token_kind ~kind:Token_kind.Bang in
  let expression = Ast.Unary (bang, Ast.Literal (Ast.Bool true)) in
  assert_parse [bang; Token.of_token_kind ~kind:Token_kind.True] expression

let expression_is_minus_number n () =
  let minus = Token.of_token_kind ~kind:Token_kind.Minus in
  let expression = Ast.Unary (minus, Ast.Literal (Ast.Number n)) in
  assert_parse
    [minus; Token.of_token_kind ~kind:(Token_kind.Number n)]
    expression

let multiplication_of_two_numbers n () =
  let infix = Token.of_token_kind ~kind:Token_kind.Star in
  let expression =
    Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
  in
  assert_parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
    expression

let division_of_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Slash in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Binary
      ( Ast.Binary
          (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
      , infix
      , Ast.Literal (Ast.Number 3.) )
  in
  assert_parse [number 1.; infix; number 2.; infix; number 3.] expression

let addition_of_two_numbers n () =
  let infix = Token.of_token_kind ~kind:Token_kind.Plus in
  let expression =
    Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
  in
  assert_parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
    expression

let substract_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Minus in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Binary
      ( Ast.Binary
          (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
      , infix
      , Ast.Literal (Ast.Number 3.) )
  in
  assert_parse [number 1.; infix; number 2.; infix; number 3.] expression

let comparison_of_two_numbers n () =
  let infix = Token.of_token_kind ~kind:Token_kind.Greater in
  let expression =
    Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
  in
  assert_parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
    expression

let comparison_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Less_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Binary
      ( Ast.Binary
          (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
      , infix
      , Ast.Literal (Ast.Number 3.) )
  in
  assert_parse [number 1.; infix; number 2.; infix; number 3.] expression

let equality_of_two_numbers n () =
  let infix = Token.of_token_kind ~kind:Token_kind.Equal_equal in
  let expression =
    Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
  in
  assert_parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
    expression

let equality_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Bang_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Binary
      ( Ast.Binary
          (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
      , infix
      , Ast.Literal (Ast.Number 3.) )
  in
  assert_parse [number 1.; infix; number 2.; infix; number 3.] expression

let all =
  [ Alcotest.test_case "expression_is_number" `Quick (expression_is_number 42.)
  ; Alcotest.test_case "expression_is_string" `Quick
      (expression_is_string "hello")
  ; Alcotest.test_case "expression_is_true_bool" `Quick expression_is_true_bool
  ; Alcotest.test_case "expression_is_false_bool" `Quick
      expression_is_false_bool
  ; Alcotest.test_case "expression_is_nil" `Quick expression_is_nil
  ; Alcotest.test_case "expression_is_identifier" `Quick
      (expression_is_identifier "x")
  ; Alcotest.test_case "expression_is_grouping" `Quick expression_is_grouping
  ; Alcotest.test_case "expression_grouping_bad_closed_paren" `Quick
      expression_grouping_bad_closed_paren
  ; Alcotest.test_case "expression_grouping_missing_closed_paren" `Quick
      expression_grouping_missing_closed_paren
  ; Alcotest.test_case "expression_is_illegal" `Quick expression_is_illegal
  ; Alcotest.test_case "expression_is_bang_unary" `Quick
      expression_is_bang_unary
  ; Alcotest.test_case "expression_is_minus_number" `Quick
      (expression_is_minus_number 42.)
  ; Alcotest.test_case "multiplication_of_two_numbers" `Quick
      (multiplication_of_two_numbers 42.)
  ; Alcotest.test_case "division_of_three_numbers" `Quick
      division_of_three_numbers
  ; Alcotest.test_case "addition_of_two_numbers" `Quick
      (addition_of_two_numbers 42.)
  ; Alcotest.test_case "substract_three_numbers" `Quick substract_three_numbers
  ; Alcotest.test_case "comparison_of_two_numbers" `Quick
      (comparison_of_two_numbers 42.)
  ; Alcotest.test_case "comparison_three_numbers" `Quick
      comparison_three_numbers
  ; Alcotest.test_case "equality_of_two_numbers" `Quick
      (equality_of_two_numbers 42.)
  ; Alcotest.test_case "equality_three_numbers" `Quick equality_three_numbers
  ]
