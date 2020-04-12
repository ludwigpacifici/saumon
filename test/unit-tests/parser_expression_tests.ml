open Base
open Saumon
module A = Alcotest_testable

let check_parse =
  Alcotest.(check (result A.ast_program string)) "check Parser.parse"

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
    ( expected_expression
    |> statement_of_expression
    |> Ast.Program.of_statement
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

let assign_a_number n () =
  let raw_identifier = "x" in
  let identifier =
    Token.of_token_kind ~kind:(Token_kind.Identifier raw_identifier)
  in
  let equal = Token.of_token_kind ~kind:Token_kind.Equal in
  let number = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Assignment
      (Ast.Identifier raw_identifier, equal, Ast.Literal (Ast.Number n))
  in
  assert_parse [identifier; equal; number] expression

let nested_assignments n () =
  let raw_identifier = "x" in
  let identifier =
    Token.of_token_kind ~kind:(Token_kind.Identifier raw_identifier)
  in
  let equal = Token.of_token_kind ~kind:Token_kind.Equal in
  let number = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Assignment
      ( Ast.Identifier raw_identifier
      , equal
      , Ast.Assignment
          (Ast.Identifier raw_identifier, equal, Ast.Literal (Ast.Number n)) )
  in
  assert_parse [identifier; equal; identifier; equal; number] expression

let assign_with_error () =
  let identifier = Token.of_token_kind ~kind:(Token_kind.Identifier "x") in
  let equal = Token.of_token_kind ~kind:Token_kind.Equal in
  let number = Token.of_token_kind ~kind:(Token_kind.Number 42.) in
  assert_parse_is_error [identifier; equal; equal; number]

let assign_with_invalid_lhs () =
  let identifier_x = Token.of_token_kind ~kind:(Token_kind.Identifier "x") in
  let identifier_y = Token.of_token_kind ~kind:(Token_kind.Identifier "y") in
  let identifier_z = Token.of_token_kind ~kind:(Token_kind.Identifier "z") in
  let plus = Token.of_token_kind ~kind:Token_kind.Plus in
  let equal = Token.of_token_kind ~kind:Token_kind.Equal in
  assert_parse_is_error [identifier_x; plus; identifier_y; equal; identifier_z]

let logic_or_of_two_numbers (n : float) () =
  let infix = Token.of_token_kind ~kind:Token_kind.Or in
  let expression =
    Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
  in
  assert_parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
    expression

let logic_and_of_two_numbers (n : float) () =
  let infix = Token.of_token_kind ~kind:Token_kind.And in
  let expression =
    Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
  in
  assert_parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
    expression

let logic_and_precedence_over_or_left () =
  let or_token = Token.of_token_kind ~kind:Token_kind.Or in
  let and_token = Token.of_token_kind ~kind:Token_kind.And in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Binary
      ( Ast.Binary
          (Ast.Literal (Ast.Number 1.), and_token, Ast.Literal (Ast.Number 2.))
      , or_token
      , Ast.Literal (Ast.Number 3.) )
  in
  (* Logical and precedence is on the left of the or *)
  assert_parse [number 1.; and_token; number 2.; or_token; number 3.] expression

let logic_and_precedence_over_or_right () =
  let or_token = Token.of_token_kind ~kind:Token_kind.Or in
  let and_token = Token.of_token_kind ~kind:Token_kind.And in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let expression =
    Ast.Binary
      ( Ast.Literal (Ast.Number 1.)
      , or_token
      , Ast.Binary
          (Ast.Literal (Ast.Number 2.), and_token, Ast.Literal (Ast.Number 3.))
      )
  in
  (* Logical and precedence is on the right of the or *)
  assert_parse [number 1.; or_token; number 2.; and_token; number 3.] expression

let all =
  [ Alcotest.test_case "Expression is number" `Quick (expression_is_number 42.)
  ; Alcotest.test_case "Expression is string" `Quick
      (expression_is_string "hello")
  ; Alcotest.test_case "Expression is true bool" `Quick expression_is_true_bool
  ; Alcotest.test_case "Expression is false bool" `Quick
      expression_is_false_bool
  ; Alcotest.test_case "Expression is nil" `Quick expression_is_nil
  ; Alcotest.test_case "Expression is identifier" `Quick
      (expression_is_identifier "x")
  ; Alcotest.test_case "Expression is grouping" `Quick expression_is_grouping
  ; Alcotest.test_case "Expression grouping bad closed paren" `Quick
      expression_grouping_bad_closed_paren
  ; Alcotest.test_case "Expression grouping missing closed paren" `Quick
      expression_grouping_missing_closed_paren
  ; Alcotest.test_case "Expression is illegal" `Quick expression_is_illegal
  ; Alcotest.test_case "Expression is bang unary" `Quick
      expression_is_bang_unary
  ; Alcotest.test_case "Expression is minus number" `Quick
      (expression_is_minus_number 42.)
  ; Alcotest.test_case "Multiplication of two numbers" `Quick
      (multiplication_of_two_numbers 42.)
  ; Alcotest.test_case "Division of three numbers" `Quick
      division_of_three_numbers
  ; Alcotest.test_case "Addition of two numbers" `Quick
      (addition_of_two_numbers 42.)
  ; Alcotest.test_case "Substract three numbers" `Quick substract_three_numbers
  ; Alcotest.test_case "Comparison of two numbers" `Quick
      (comparison_of_two_numbers 42.)
  ; Alcotest.test_case "Comparison three numbers" `Quick
      comparison_three_numbers
  ; Alcotest.test_case "Equality of two numbers" `Quick
      (equality_of_two_numbers 42.)
  ; Alcotest.test_case "Equality three numbers" `Quick equality_three_numbers
  ; Alcotest.test_case "Assign a number" `Quick (assign_a_number 42.)
  ; Alcotest.test_case "Nested assignments" `Quick (nested_assignments 42.)
  ; Alcotest.test_case "Assign with error" `Quick assign_with_error
  ; Alcotest.test_case "Assign with invalid lhs" `Quick assign_with_invalid_lhs
  ; Alcotest.test_case "Logic or of two numbers" `Quick
      (logic_or_of_two_numbers 42.)
  ; Alcotest.test_case "Logic and of two numbers" `Quick
      (logic_and_of_two_numbers 42.)
  ; Alcotest.test_case "Logic and precedence over or left" `Quick
      logic_and_precedence_over_or_left
  ; Alcotest.test_case "Logic and precedence over or right" `Quick
      logic_and_precedence_over_or_right ]
