open Base
open OUnit2
open Saumon
open Test_utils

let f () = ()

let var = Token.of_token_kind ~kind:Token_kind.Var

let raw_identifier = "x"

let identifier =
  Token.of_token_kind ~kind:(Token_kind.Identifier raw_identifier)

let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon

let valid_declaration_with_no_initialization () =
  Parser.parse [var; identifier; semicolon]
  === ( (var, Ast.Identifier raw_identifier, None, semicolon)
      |> Ast.Program.of_declaration |> Result.return )

let equal = Token.of_token_kind ~kind:Token_kind.Equal

let raw_value = 5.

let value = Token.of_token_kind ~kind:(Token_kind.Number raw_value)

let valid_declaration_with_initialization () =
  Parser.parse [var; identifier; equal; value; semicolon]
  === ( ( var
        , Ast.Identifier raw_identifier
        , Some (equal, Ast.Literal (Ast.Number raw_value))
        , semicolon )
      |> Ast.Program.of_declaration |> Result.return )

let no_declaration () = Parser.parse [] === Ok Ast.Program.empty

let empty_token_list () =
  let eof = Token.of_token_kind ~kind:Token_kind.Eof in
  Parser.parse [eof] === Ok Ast.Program.empty

let only_var_keyword_is_error () =
  Parser.parse [var] |> Result.is_error === true

let missing_initialization_or_semicolon_is_error () =
  Parser.parse [var; identifier] |> Result.is_error === true

let not_equal_after_identifier_is_error () =
  Parser.parse [var; identifier; var] |> Result.is_error === true

let missing_initialization_after_equal_is_error () =
  Parser.parse [var; identifier; equal] |> Result.is_error === true

let not_expression_afer_equal_is_error () =
  Parser.parse [var; identifier; equal; identifier] |> Result.is_error === true

let missing_semicolon_after_initialization_is_error () =
  Parser.parse [var; identifier; equal; value] |> Result.is_error === true

let not_semicolon_after_initialization_is_error () =
  Parser.parse [var; identifier; equal; value; var] |> Result.is_error === true

let parser_declaration_tests =
  [ ( "Valid declaration with no initialization"
    >:: fun _ -> valid_declaration_with_no_initialization () )
  ; ( "Valid declaration with initialization"
    >:: fun _ -> valid_declaration_with_initialization () )
  ; ("No declaration" >:: fun _ -> no_declaration ())
  ; ("Empty token list" >:: fun _ -> empty_token_list ())
  ; ("Only var keyword is error" >:: fun _ -> only_var_keyword_is_error ())
  ; ( "Missing initialization or semicolon is error"
    >:: fun _ -> missing_initialization_or_semicolon_is_error () )
  ; ( "Not equal after identifier is error"
    >:: fun _ -> not_equal_after_identifier_is_error () )
  ; ( "Missing initialization after equal is error"
    >:: fun _ -> missing_initialization_after_equal_is_error () )
  ; ( "Not expression afer equal is error"
    >:: fun _ -> not_expression_afer_equal_is_error () )
  ; ( "Missing semicolon after initialization is error"
    >:: fun _ -> missing_semicolon_after_initialization_is_error () )
  ; ( "Not semicolon after initialization is error"
    >:: fun _ -> not_semicolon_after_initialization_is_error () ) ]
