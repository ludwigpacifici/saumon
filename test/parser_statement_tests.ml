open OUnit2
open Test_utils
open Saumon
open Base

let noop_when_no_token () = Parser.parse [] === Ok Ast.Program.empty

let noop_when_eof () =
  Parser.parse [Token.of_token_kind ~kind:Token_kind.Eof]
  === Ok Ast.Program.empty

let error_when_token_after_eof () =
  Parser.parse
    [ Token.of_token_kind ~kind:Token_kind.Eof
    ; Token.of_token_kind ~kind:Token_kind.Semicolon ]
  |> Result.is_error === true

let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon

let noop_when_semicolon () = Parser.parse [semicolon] === Ok Ast.Program.empty

let valid_expression_statement () =
  Parser.parse [Token.of_token_kind ~kind:Token_kind.Nil; semicolon]
  === ( Ast.Expression_statement (Ast.Literal Ast.Nil, semicolon)
      |> Ast.Program.of_statement |> Result.return )

let semicolon_missing_after_expression_statement () =
  Parser.parse [Token.of_token_kind ~kind:Token_kind.Nil]
  |> Result.is_error === true

let print = Token.of_token_kind ~kind:Token_kind.Print

let valid_print_statement () =
  Parser.parse [print; Token.of_token_kind ~kind:Token_kind.Nil; semicolon]
  === ( Ast.Print_statement (print, Ast.Literal Ast.Nil, semicolon)
      |> Ast.Program.of_statement |> Result.return )

let semicolon_missing_after_print_statement () =
  Parser.parse [print; Token.of_token_kind ~kind:Token_kind.Nil]
  |> Result.is_error === true

let parser_statement_tests =
  [ ("Noop when no token" >:: fun _ -> noop_when_no_token ())
  ; ("Noop when eof" >:: fun _ -> noop_when_eof ())
  ; ("Error when token after eof" >:: fun _ -> error_when_token_after_eof ())
  ; ("Noop when semicolon" >:: fun _ -> noop_when_semicolon ())
  ; ("Valid expression statement" >:: fun _ -> valid_expression_statement ())
  ; ( "Semicolon missing after expression statement"
    >:: fun _ -> semicolon_missing_after_expression_statement () )
  ; ("Valid print statement" >:: fun _ -> valid_print_statement ())
  ; ( "Semicolon missing after print statement"
    >:: fun _ -> semicolon_missing_after_print_statement () ) ]
