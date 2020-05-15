open Base
open Saumon
module A = Alcotest_testable

let check_true = Alcotest.(check bool) "check Parser.parse" true

let check_parse =
  Alcotest.(check (result A.ast_program string)) "check Parser.parse"

let left_brace = Token.of_token_kind ~kind:Token_kind.Left_brace

let right_brace = Token.of_token_kind ~kind:Token_kind.Right_brace

let empty_block () =
  check_parse
    (Parser.parse [left_brace; right_brace])
    (Ast.Block [] |> Ast.Program.of_statement |> Result.return)

let not_closed_block () =
  check_true (Parser.parse [left_brace] |> Result.is_error)

let not_opened_block () =
  check_true (Parser.parse [right_brace] |> Result.is_error)

let block_with_several_statements () =
  let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon in
  let raw_value = 1. in
  check_parse
    (Parser.parse
       [ left_brace
       ; Token.of_token_kind ~kind:Token_kind.Nil
       ; semicolon
       ; Token.of_token_kind ~kind:(Token_kind.Number raw_value)
       ; semicolon
       ; right_brace ])
    ( Ast.Block
        [ Ast.Statement (Ast.Expression_statement (Ast.Literal Ast.Nil))
        ; Ast.Statement
            (Ast.Expression_statement (Ast.Literal (Ast.Number raw_value))) ]
    |> Ast.Program.of_statement
    |> Result.return )

let all =
  [ Alcotest.test_case "Empty block" `Quick empty_block
  ; Alcotest.test_case "Not closed block" `Quick not_closed_block
  ; Alcotest.test_case "Not opened block" `Quick not_opened_block
  ; Alcotest.test_case "Block with several statements" `Quick
      block_with_several_statements ]
