open Base
open Saumon
module A = Alcotest_testable

let check_true = Alcotest.(check bool) "check Parser.parse" true

let check_parse =
  Alcotest.(check (result A.ast_program string)) "check Parser.parse"

let left_paren = Token.of_token_kind ~kind:Token_kind.Left_paren
let right_paren = Token.of_token_kind ~kind:Token_kind.Right_paren
let for_token = Token.of_token_kind ~kind:Token_kind.For
let var = Token.of_token_kind ~kind:Token_kind.Var
let true_token = Token.of_token_kind ~kind:Token_kind.True
let print = Token.of_token_kind ~kind:Token_kind.Print
let equal = Token.of_token_kind ~kind:Token_kind.Equal
let plus = Token.of_token_kind ~kind:Token_kind.Plus
let semicolon = Token.of_token_kind ~kind:Token_kind.Semicolon
let raw_identifier = "i"

let identifier =
  Token.of_token_kind ~kind:(Token_kind.Identifier raw_identifier)

let zero = Token.of_token_kind ~kind:(Token_kind.Number 0.)
let one = Token.of_token_kind ~kind:(Token_kind.Number 1.)
let three = Token.of_token_kind ~kind:(Token_kind.Number 3.)
let less = Token.of_token_kind ~kind:Token_kind.Less
let nil = Token.of_token_kind ~kind:Token_kind.Nil

let valid_for () =
  check_parse
    (Parser.parse
       (* for ( var i = 0 ; i < 3 ; i = i + 1 ) print i; *)
       [ for_token; left_paren; var; identifier; equal; zero; semicolon
       ; identifier; less; three; semicolon; identifier; equal; identifier; plus
       ; one; right_paren; print; identifier; semicolon ])
    ( Ast.Block
        { statements=
            [ Ast.Variable_declaration
                { var
                ; identifier= Ast.Identifier raw_identifier
                ; assign= Some {equal; expr= Ast.Literal (Ast.Number 0.)}
                ; semicolon }
            ; Ast.Statement
                (Ast.While_statement
                   { condition=
                       Ast.Binary
                         { left_expr= Ast.Literal (Ast.Identifier raw_identifier)
                         ; operator= less
                         ; right_expr= Ast.Literal (Ast.Number 3.) }
                   ; body=
                       Ast.Block
                         { statements=
                             [ Ast.Statement
                                 (Ast.Print_statement
                                    { expr=
                                        Ast.Literal
                                          (Ast.Identifier raw_identifier) })
                             ; Ast.Statement
                                 (Ast.Expression_statement
                                    { expr=
                                        Ast.Assignment
                                          { identifier=
                                              Ast.Identifier raw_identifier
                                          ; equal
                                          ; expr=
                                              Ast.Binary
                                                { left_expr=
                                                    Ast.Literal
                                                      (Ast.Identifier
                                                         raw_identifier)
                                                ; operator= plus
                                                ; right_expr=
                                                    Ast.Literal (Ast.Number 1.)
                                                } } }) ] } }) ] }
    |> Ast.Program.of_statement |> Result.return )

let valid_for_without_exit_condition () =
  check_parse
    (Parser.parse
       (* for ( var i = 0 ; ; i = i + 1 ) print i; *)
       [ for_token; left_paren; var; identifier; equal; zero; semicolon
       ; semicolon; identifier; equal; identifier; plus; one; right_paren; print
       ; identifier; semicolon ])
    ( Ast.Block
        { statements=
            [ Ast.Variable_declaration
                { var
                ; identifier= Ast.Identifier raw_identifier
                ; assign= Some {equal; expr= Ast.Literal (Ast.Number 0.)}
                ; semicolon }
            ; Ast.Statement
                (Ast.While_statement
                   { condition= Ast.Literal (Ast.Bool true)
                   ; body=
                       Ast.Block
                         { statements=
                             [ Ast.Statement
                                 (Ast.Print_statement
                                    { expr=
                                        Ast.Literal
                                          (Ast.Identifier raw_identifier) })
                             ; Ast.Statement
                                 (Ast.Expression_statement
                                    { expr=
                                        Ast.Assignment
                                          { identifier=
                                              Ast.Identifier raw_identifier
                                          ; equal
                                          ; expr=
                                              Ast.Binary
                                                { left_expr=
                                                    Ast.Literal
                                                      (Ast.Identifier
                                                         raw_identifier)
                                                ; operator= plus
                                                ; right_expr=
                                                    Ast.Literal (Ast.Number 1.)
                                                } } }) ] } }) ] }
    |> Ast.Program.of_statement |> Result.return )

let valid_for_without_increment () =
  check_parse
    (Parser.parse
       (* for ( var i = 0 ; i < 3 ; ) print i; *)
       [ for_token; left_paren; var; identifier; equal; zero; semicolon
       ; identifier; less; three; semicolon; right_paren; print; identifier
       ; semicolon ])
    ( Ast.Block
        { statements=
            [ Ast.Variable_declaration
                { var
                ; identifier= Ast.Identifier raw_identifier
                ; assign= Some {equal; expr= Ast.Literal (Ast.Number 0.)}
                ; semicolon }
            ; Ast.Statement
                (Ast.While_statement
                   { condition=
                       Ast.Binary
                         { left_expr= Ast.Literal (Ast.Identifier raw_identifier)
                         ; operator= less
                         ; right_expr= Ast.Literal (Ast.Number 3.) }
                   ; body=
                       Ast.Block
                         { statements=
                             [ Ast.Statement
                                 (Ast.Print_statement
                                    { expr=
                                        Ast.Literal
                                          (Ast.Identifier raw_identifier) }) ]
                         } }) ] }
    |> Ast.Program.of_statement |> Result.return )

let valid_for_minimal () =
  check_parse
    (Parser.parse
       (* for ( nil; ; ) nil; *)
       [ for_token; left_paren; nil; semicolon; semicolon; right_paren; nil
       ; semicolon ])
    ( Ast.Block
        { statements=
            [ Ast.Statement
                (Ast.Expression_statement {expr= Ast.Literal Ast.Nil})
            ; Ast.Statement
                (Ast.While_statement
                   { condition= Ast.Literal (Ast.Bool true)
                   ; body=
                       Ast.Block
                         { statements=
                             [ Ast.Statement
                                 (Ast.Expression_statement
                                    {expr= Ast.Literal Ast.Nil}) ] } }) ] }
    |> Ast.Program.of_statement |> Result.return )

let missing_for_token () =
  check_true
    ( Parser.parse
        (* ( nil ; ; ) nil; *)
        [left_paren; nil; semicolon; semicolon; right_paren; nil; semicolon]
    |> Result.is_error )

let missing_left_paren () =
  check_true
    ( Parser.parse
        (* for nil ; ; ) nil; *)
        [for_token; nil; semicolon; semicolon; right_paren; nil; semicolon]
    |> Result.is_error )

let missing_one_semicolon () =
  check_true
    ( Parser.parse
        (* for ( nil ; ) nil; *)
        [for_token; left_paren; nil; semicolon; right_paren; nil; semicolon]
    |> Result.is_error )

let missing_two_semicolons () =
  check_true
    ( Parser.parse
        (* for ( nil ) nil; *)
        [for_token; left_paren; nil; right_paren; nil; semicolon]
    |> Result.is_error )

let missing_right_paren () =
  check_true
    ( Parser.parse
        (* for ( nil; ; nil; *)
        [for_token; left_paren; nil; semicolon; semicolon; nil; semicolon]
    |> Result.is_error )

let missing_body () =
  check_true
    ( Parser.parse
        (* for nil; ; ) *)
        [for_token; left_paren; nil; semicolon; semicolon; right_paren]
    |> Result.is_error )

let all =
  [ Alcotest.test_case "Valid for" `Quick valid_for
  ; Alcotest.test_case "Valid for without exit condition" `Quick
      valid_for_without_exit_condition
  ; Alcotest.test_case "Valid for without increment" `Quick
      valid_for_without_increment
  ; Alcotest.test_case "Valid for minimal" `Quick valid_for_minimal
  ; Alcotest.test_case "Missilg for token" `Quick missing_for_token
  ; Alcotest.test_case "Missing left paren" `Quick missing_left_paren
  ; Alcotest.test_case "Missing one semicolon" `Quick missing_one_semicolon
  ; Alcotest.test_case "Missing two semicolon" `Quick missing_two_semicolons
  ; Alcotest.test_case "Missing right paren" `Quick missing_right_paren
  ; Alcotest.test_case "Missing body" `Quick missing_body ]
