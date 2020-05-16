open Base
open Saumon
module A = Alcotest_testable

let check_true = Alcotest.(check bool) "check Parser.parse" true

let check_parse =
  Alcotest.(check (result A.ast_program string)) "check Parser.parse"

let left_paren = Token.of_token_kind ~kind:Token_kind.Left_paren

let right_paren = Token.of_token_kind ~kind:Token_kind.Right_paren

let while_token = Token.of_token_kind ~kind:Token_kind.While

let true_token = Token.of_token_kind ~kind:Token_kind.True

let nil = Token.of_token_kind ~kind:Token_kind.Nil

let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon

let valid_while () =
  check_parse
    (Parser.parse
       (* while ( true ) nil ; *)
       [while_token; left_paren; true_token; right_paren; nil; semicolon])
    ( Ast.While_statement
        { condition = Ast.Literal (Ast.Bool true)
        ; body = Ast.Expression_statement {expr = Ast.Literal Ast.Nil} }
    |> Ast.Program.of_statement
    |> Result.return )

let invalid_while_with_no_condition () =
  check_true
    ( Parser.parse
        (* while ( ) nil ; *)
        [while_token; left_paren; right_paren; nil; semicolon]
    |> Result.is_error )

let invalid_while_with_not_a_condition () =
  check_true
    ( Parser.parse
        (* while ( nil ; ) nil ; *)
        [while_token; left_paren; nil; semicolon; right_paren; nil; semicolon]
    |> Result.is_error )

let invalid_while_with_no_left_paren () =
  check_true
    ( Parser.parse
        (* while true ) nil ; *)
        [while_token; true_token; right_paren; nil; semicolon]
    |> Result.is_error )

let invalid_while_with_no_right_paren () =
  check_true
    ( Parser.parse
        (* while ( true nil ; *)
        [while_token; left_paren; true_token; nil; semicolon]
    |> Result.is_error )

let invalid_while_with_no_while_body () =
  check_true
    ( Parser.parse (* while ( true ) *) [while_token; left_paren; true_token]
    |> Result.is_error )

let all =
  [ Alcotest.test_case "Valid while" `Quick valid_while
  ; Alcotest.test_case "Invalid while with no condition" `Quick
      invalid_while_with_no_condition
  ; Alcotest.test_case "Invalid while with not a condition" `Quick
      invalid_while_with_not_a_condition
  ; Alcotest.test_case "Invalid while with no left paren" `Quick
      invalid_while_with_no_left_paren
  ; Alcotest.test_case "Invalid while with no right paren" `Quick
      invalid_while_with_no_right_paren
  ; Alcotest.test_case "Invalid while with no while body" `Quick
      invalid_while_with_no_while_body ]
