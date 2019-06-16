open OUnit2
open Test_utils
open Saumon
open Base

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
  parse_with_semicolon ts
  === ( expected_expression |> statement_of_expression
      |> Ast.Program.of_statement |> Result.return )

(* Helper that assert a token list will be parsed and generate errors *)
let assert_parse_is_error ts =
  parse_with_semicolon ts |> Result.is_error === true

let expression_is_number inner =
  let expression = Ast.Literal (Ast.Number inner) in
  assert_parse [Token.of_token_kind ~kind:(Token_kind.Number inner)] expression

let expression_is_string inner =
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

let expression_is_minus_number n =
  let minus = Token.of_token_kind ~kind:Token_kind.Minus in
  let expression = Ast.Unary (minus, Ast.Literal (Ast.Number n)) in
  assert_parse
    [minus; Token.of_token_kind ~kind:(Token_kind.Number n)]
    expression

let multiplication_of_two_numbers n =
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

let addition_of_two_numbers n =
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

let comparison_of_two_numbers n =
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

let equality_of_two_numbers n =
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

let parser_expression_tests =
  [ ("Expression is number" >:: fun _ -> expression_is_number 42.)
  ; ("Expression is string" >:: fun _ -> expression_is_string "hello")
  ; ("Expression is true bool" >:: fun _ -> expression_is_true_bool ())
  ; ("Expression is false bool" >:: fun _ -> expression_is_false_bool ())
  ; ("Expression is nil" >:: fun _ -> expression_is_nil ())
  ; ("Expression is grouping" >:: fun _ -> expression_is_grouping ())
  ; ( "Expression grouping bad closed paren"
    >:: fun _ -> expression_grouping_bad_closed_paren () )
  ; ( "Expression grouping missing closed paren"
    >:: fun _ -> expression_grouping_missing_closed_paren () )
  ; ("Expression is illegal" >:: fun _ -> expression_is_illegal ())
  ; ("Expression is bang unary" >:: fun _ -> expression_is_bang_unary ())
  ; ("Expression is minus number" >:: fun _ -> expression_is_minus_number 42.)
  ; ( "Multiplication of two numbers"
    >:: fun _ -> multiplication_of_two_numbers 42. )
  ; ("Division of three numbers" >:: fun _ -> division_of_three_numbers ())
  ; ("Addition of two numbers" >:: fun _ -> addition_of_two_numbers 42.)
  ; ("Substract three numbers" >:: fun _ -> substract_three_numbers ())
  ; ("Comparison of two numbers" >:: fun _ -> comparison_of_two_numbers 42.)
  ; ("Comparison three numbers" >:: fun _ -> comparison_three_numbers ())
  ; ("Equality of two numbers" >:: fun _ -> equality_of_two_numbers 42.)
  ; ("Equality three numbers" >:: fun _ -> equality_three_numbers ()) ]
