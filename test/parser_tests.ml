open OUnit2
open Test_utils
open Saumon

let expression_is_number inner =
  Parser.parse [Token.of_token_kind ~kind:(Token_kind.Number inner)]
  === Ok (Ast.Literal (Ast.Number inner))

let expression_is_string inner =
  Parser.parse [Token.of_token_kind ~kind:(Token_kind.String inner)]
  === Ok (Ast.Literal (Ast.String inner))

let expression_is_true_bool () =
  Parser.parse [Token.of_token_kind ~kind:Token_kind.True]
  === Ok (Ast.Literal (Ast.Bool true))

let expression_is_false_bool () =
  Parser.parse [Token.of_token_kind ~kind:Token_kind.False]
  === Ok (Ast.Literal (Ast.Bool false))

let expression_is_nil () =
  Parser.parse [Token.of_token_kind ~kind:Token_kind.Nil]
  === Ok (Ast.Literal Ast.Nil)

let expression_is_grouping () =
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  let right = Token.of_token_kind ~kind:Token_kind.Right_paren in
  Parser.parse [left; Token.of_token_kind ~kind:Token_kind.True; right]
  === Ok (Ast.Grouping (left, Ast.Literal (Ast.Bool true), right))

let expression_grouping_bad_closed_paren () =
  let open Base in
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  Parser.parse [left; Token.of_token_kind ~kind:Token_kind.True; left]
  |> Result.is_error === true

let expression_grouping_missing_closed_paren () =
  let open Base in
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  Parser.parse [left; Token.of_token_kind ~kind:Token_kind.True]
  |> Result.is_error === true

let expression_is_illegal () =
  let open Base in
  Parser.parse [Token.of_token_kind ~kind:Token_kind.Return]
  |> Result.is_error === true

let expression_is_empty () =
  let open Base in
  Parser.parse [] |> Result.is_error === true

let expression_is_bang_unary () =
  let bang = Token.of_token_kind ~kind:Token_kind.Bang in
  Parser.parse [bang; Token.of_token_kind ~kind:Token_kind.True]
  === Ok (Ast.Unary (bang, Ast.Literal (Ast.Bool true)))

let expression_is_minus_number n =
  let minus = Token.of_token_kind ~kind:Token_kind.Minus in
  Parser.parse [minus; Token.of_token_kind ~kind:(Token_kind.Number n)]
  === Ok (Ast.Unary (minus, Ast.Literal (Ast.Number n)))

let multiplication_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Star in
  Parser.parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  === Ok
        (Ast.Binary
           (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let division_of_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Slash in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ok (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.))) in
  actual === expect

let addition_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Plus in
  Parser.parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  === Ok
        (Ast.Binary
           (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let substract_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Minus in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ok (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.))) in
  actual === expect

let addition_tests =
  [ ("Addition of two numbers" >:: fun _ -> addition_of_two_numbers 42.)
  ; ("Substract three numbers" >:: fun _ -> substract_three_numbers ()) ]

let comparison_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Greater in
  Parser.parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  === Ok
        (Ast.Binary
           (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let comparison_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Less_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ok (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.))) in
  actual === expect

let equality_of_two_numbers n =
  let infix = Token.of_token_kind ~kind:Token_kind.Equal_equal in
  Parser.parse
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  === Ok
        (Ast.Binary
           (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n)))

let equality_three_numbers () =
  let infix = Token.of_token_kind ~kind:Token_kind.Bang_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = Parser.parse [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = Ok (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.))) in
  actual === expect

let parser_tests =
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
  ; ("Expression is empty" >:: fun _ -> expression_is_empty ())
  ; ("Expression is bang unary" >:: fun _ -> expression_is_bang_unary ())
  ; ("Expression is minus number" >:: fun _ -> expression_is_minus_number 42.)
  ; ( "Multiplication of two numbers"
    >:: fun _ -> multiplication_of_two_numbers 42. )
  ; ("Division of three numbers" >:: fun _ -> division_of_three_numbers ())
  ; ("Comparison of two numbers" >:: fun _ -> comparison_of_two_numbers 42.)
  ; ("Comparison three numbers" >:: fun _ -> comparison_three_numbers ())
  ; ("Equality of two numbers" >:: fun _ -> equality_of_two_numbers 42.)
  ; ("Equality three numbers" >:: fun _ -> equality_three_numbers ()) ]