open Base
open Saumon
module A = Alcotest_testable

let check_true = Alcotest.(check bool) "check Parser.parse" true

let check_parse =
  Alcotest.(check (result A.ast_program string)) "check Parser.parse"

let left_paren = Token.of_token_kind ~kind:Token_kind.Left_paren

let right_paren = Token.of_token_kind ~kind:Token_kind.Right_paren

let if_token = Token.of_token_kind ~kind:Token_kind.If

let else_token = Token.of_token_kind ~kind:Token_kind.Else

let true_token = Token.of_token_kind ~kind:Token_kind.True

let false_token = Token.of_token_kind ~kind:Token_kind.False

let nil = Token.of_token_kind ~kind:Token_kind.Nil

let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon

let valid_if () =
  check_parse
    (Parser.parse
       (* if ( true ) nil ; *)
       [if_token; left_paren; true_token; right_paren; nil; semicolon])
    ( Ast.If_statement
        ( if_token
        , left_paren
        , Ast.Literal (Ast.Bool true)
        , right_paren
        , Ast.Expression_statement (Ast.Literal Ast.Nil, semicolon)
        , None )
    |> Ast.Program.of_statement
    |> Result.return )

let valid_if_else () =
  check_parse
    (Parser.parse
       (* if ( true ) nil ; else nil ; *)
       [ if_token
       ; left_paren
       ; true_token
       ; right_paren
       ; nil
       ; semicolon
       ; else_token
       ; nil
       ; semicolon ])
    ( Ast.If_statement
        ( if_token
        , left_paren
        , Ast.Literal (Ast.Bool true)
        , right_paren
        , Ast.Expression_statement (Ast.Literal Ast.Nil, semicolon)
        , Some
            ( else_token
            , Ast.Expression_statement (Ast.Literal Ast.Nil, semicolon) ) )
    |> Ast.Program.of_statement
    |> Result.return )

let invalid_if_with_no_condition () =
  check_true
    ( Parser.parse
        (* if ( ) nil ; *)
        [if_token; left_paren; right_paren; nil; semicolon]
    |> Result.is_error )

let invalid_if_with_not_a_condition () =
  check_true
    ( Parser.parse
        (* if ( nil ; ) nil ; *)
        [if_token; left_paren; nil; semicolon; right_paren; nil; semicolon]
    |> Result.is_error )

let invalid_if_with_no_left_paren () =
  check_true
    ( Parser.parse
        (* if true ) nil ; *) [if_token; true_token; right_paren; nil; semicolon]
    |> Result.is_error )

let invalid_if_with_no_right_paren () =
  check_true
    ( Parser.parse
        (* if ( true nil ; *) [if_token; left_paren; true_token; nil; semicolon]
    |> Result.is_error )

let invalid_if_with_no_if_body () =
  check_true
    ( Parser.parse (* if ( true ) *) [if_token; left_paren; true_token]
    |> Result.is_error )

let invalid_if_else_with_no_if_body () =
  check_true
    ( Parser.parse
        (* if ( true ) else nil ; *)
        [if_token; left_paren; true_token; else_token; nil; semicolon]
    |> Result.is_error )

let invalid_if_else_with_no_else_body () =
  check_true
    ( Parser.parse
        (* if ( true ) nil; else *)
        [if_token; left_paren; true_token; else_token]
    |> Result.is_error )

let all =
  [ Alcotest.test_case "Valid if conditional" `Quick valid_if
  ; Alcotest.test_case "Valid if else conditional" `Quick valid_if_else
  ; Alcotest.test_case "Invalid if with no condition" `Quick
      invalid_if_with_no_condition
  ; Alcotest.test_case "Invalid if with not a condition" `Quick
      invalid_if_with_not_a_condition
  ; Alcotest.test_case "Invalid if with no left paren" `Quick
      invalid_if_with_no_left_paren
  ; Alcotest.test_case "Invalid if with no right paren" `Quick
      invalid_if_with_no_right_paren
  ; Alcotest.test_case "Invalid if with no if body" `Quick
      invalid_if_with_no_if_body
  ; Alcotest.test_case "Invalid if else with no if body" `Quick
      invalid_if_else_with_no_if_body
  ; Alcotest.test_case "Invalid if else with no else body" `Quick
      invalid_if_else_with_no_else_body ]
