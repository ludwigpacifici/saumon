open Base
open Saumon
module A = Alcotest_testable

let check_parse = Alcotest.(check (result A.ast_program string)) "check Parser.parse"

let check_true = Alcotest.(check bool) "check Parser.parse" true

let noop_when_no_token () = check_parse (Parser.parse []) (Ok Ast.Program.empty)

let noop_when_eof () = check_parse (Parser.parse [Token.of_token_kind ~kind:Token_kind.Eof]) (Ok Ast.Program.empty)

let error_when_token_after_eof () =
  check_true
    ( Parser.parse [Token.of_token_kind ~kind:Token_kind.Eof; Token.of_token_kind ~kind:Token_kind.Semicolon]
    |> Result.is_error )

let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon

let error_when_semicolon () = check_true (Parser.parse [semicolon] |> Result.is_error)

let valid_expression_statement () =
  check_parse
    (Parser.parse [Token.of_token_kind ~kind:Token_kind.Nil; semicolon])
    (Ast.Expression_statement {expr = Ast.Literal Ast.Nil} |> Ast.Program.of_statement |> Result.return)

let semicolon_missing_after_expression_statement () =
  check_true (Parser.parse [Token.of_token_kind ~kind:Token_kind.Nil] |> Result.is_error)

let print = Token.of_token_kind ~kind:Token_kind.Print

let valid_print_statement () =
  check_parse
    (Parser.parse [print; Token.of_token_kind ~kind:Token_kind.Nil; semicolon])
    (Ast.Print_statement {expr = Ast.Literal Ast.Nil} |> Ast.Program.of_statement |> Result.return)

let semicolon_missing_after_print_statement () =
  check_true (Parser.parse [print; Token.of_token_kind ~kind:Token_kind.Nil] |> Result.is_error)

let all =
  [ Alcotest.test_case "Noop when no token" `Quick noop_when_no_token
  ; Alcotest.test_case "Noop when eof" `Quick noop_when_eof
  ; Alcotest.test_case "Error when token after eof" `Quick error_when_token_after_eof
  ; Alcotest.test_case "Error when semicolon" `Quick error_when_semicolon
  ; Alcotest.test_case "Valid expression statement" `Quick valid_expression_statement
  ; Alcotest.test_case "Semicolon missing after expression statement" `Quick
      semicolon_missing_after_expression_statement
  ; Alcotest.test_case "Valid print statement" `Quick valid_print_statement
  ; Alcotest.test_case "Semicolon missing after print statement" `Quick semicolon_missing_after_print_statement ]
