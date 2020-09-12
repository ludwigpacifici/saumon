open Base
open Saumon
module A = Alcotest_testable

let check_parse =
  Alcotest.(check (result A.ast_program string)) "check Parser.parse"

let check_true = Alcotest.(check bool) "check Parser.parse" true
let var = Token.of_token_kind ~kind:Token_kind.Var
let raw_identifier = "x"

let identifier =
  Token.of_token_kind ~kind:(Token_kind.Identifier raw_identifier)

let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon

let valid_declaration_with_no_initialization () =
  check_parse
    (Parser.parse [var; identifier; semicolon])
    ( Ast.Variable_declaration
        {var; identifier= Ast.Identifier raw_identifier; assign= None; semicolon}
    |> Ast.Program.of_declaration |> Result.return )

let equal = Token.of_token_kind ~kind:Token_kind.Equal
let raw_value = 5.
let value = Token.of_token_kind ~kind:(Token_kind.Number raw_value)

let valid_declaration_with_initialization () =
  check_parse
    (Parser.parse [var; identifier; equal; value; semicolon])
    ( Ast.Variable_declaration
        { var
        ; identifier= Ast.Identifier raw_identifier
        ; assign= Some {equal; expr= Ast.Literal (Ast.Number raw_value)}
        ; semicolon }
    |> Ast.Program.of_declaration |> Result.return )

let plus = Token.of_token_kind ~kind:Token_kind.Plus

let valid_declaration_with_expression () =
  check_parse
    (Parser.parse [var; identifier; equal; value; plus; value; semicolon])
    ( Ast.Variable_declaration
        { var
        ; identifier= Ast.Identifier raw_identifier
        ; assign=
            Some
              { equal
              ; expr=
                  Ast.Binary
                    { left_expr= Ast.Literal (Ast.Number 5.)
                    ; operator=
                        { Token.kind= Token_kind.Plus
                        ; location= {Location.line= 1; column= 0} }
                    ; right_expr= Ast.Literal (Ast.Number 5.) } }
        ; semicolon }
    |> Ast.Program.of_declaration |> Result.return )

let no_declaration () = check_parse (Parser.parse []) (Ok Ast.Program.empty)

let empty_token_list () =
  let eof = Token.of_token_kind ~kind:Token_kind.Eof in
  check_parse (Parser.parse [eof]) (Ok Ast.Program.empty)

let only_var_keyword_is_error () =
  check_true (Parser.parse [var] |> Result.is_error)

let missing_initialization_or_semicolon_is_error () =
  check_true (Parser.parse [var; identifier] |> Result.is_error)

let not_equal_after_identifier_is_error () =
  check_true (Parser.parse [var; identifier; var] |> Result.is_error)

let missing_initialization_after_equal_is_error () =
  check_true (Parser.parse [var; identifier; equal] |> Result.is_error)

let not_expression_afer_equal_is_error () =
  check_true
    (Parser.parse [var; identifier; equal; identifier] |> Result.is_error)

let missing_semicolon_after_initialization_is_error () =
  check_true (Parser.parse [var; identifier; equal; value] |> Result.is_error)

let not_semicolon_after_initialization_is_error () =
  check_true
    (Parser.parse [var; identifier; equal; value; var] |> Result.is_error)

let all =
  [ Alcotest.test_case "Valid declaration with no initialization" `Quick
      valid_declaration_with_no_initialization
  ; Alcotest.test_case "Valid declaration with initialization" `Quick
      valid_declaration_with_initialization
  ; Alcotest.test_case "Valid declaration with expression" `Quick
      valid_declaration_with_expression
  ; Alcotest.test_case "No declaration" `Quick no_declaration
  ; Alcotest.test_case "Empty token list" `Quick empty_token_list
  ; Alcotest.test_case "Only var keyword is error" `Quick
      only_var_keyword_is_error
  ; Alcotest.test_case "Missing initialization or semicolon is error" `Quick
      missing_initialization_or_semicolon_is_error
  ; Alcotest.test_case "Not equal after identifier is error" `Quick
      not_equal_after_identifier_is_error
  ; Alcotest.test_case "Missing initialization after equal is error" `Quick
      missing_initialization_after_equal_is_error
  ; Alcotest.test_case "Not expression afer equal is error" `Quick
      not_expression_afer_equal_is_error
  ; Alcotest.test_case "Missing semicolon after initialization is error" `Quick
      missing_semicolon_after_initialization_is_error
  ; Alcotest.test_case "Not semicolon after initialization is error" `Quick
      not_semicolon_after_initialization_is_error ]
