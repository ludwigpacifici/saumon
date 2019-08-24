open Base
open Saumon
module A = Alcotest_testable

let check_value =
  Alcotest.(check (result A.value A.value_error)) "check Interpreter.evaluate"

let check_true =
  Alcotest.(check bool) "check Interpreter.evaluate is error" true

let evaluate = Interpreter.evaluate (Environment.empty ())

let evaluate_literal () =
  check_value (evaluate (Ast.Literal (Ast.Number 42.))) (Ok (Value.Number 42.)) ;
  check_value
    (evaluate (Ast.Literal (Ast.String "hello")))
    (Ok (Value.String "hello")) ;
  check_value (evaluate (Ast.Literal (Ast.Bool true))) (Ok (Value.Bool true)) ;
  check_value (evaluate (Ast.Literal (Ast.Bool false))) (Ok (Value.Bool false)) ;
  check_value (evaluate (Ast.Literal Ast.Nil)) (Ok Value.Nil)

let evaluate_grouping () =
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  let right = Token.of_token_kind ~kind:Token_kind.Right_paren in
  check_value
    (evaluate (Ast.Grouping (left, Ast.Literal (Ast.Number 42.), right)))
    (Ok (Value.Number 42.))

let evaluate_unary () =
  check_value
    (evaluate
       (Ast.Unary
          ( Token.of_token_kind ~kind:Token_kind.Minus
          , Ast.Literal (Ast.Number 42.) )))
    (Ok (Value.Number (-42.))) ;
  check_true
    ( evaluate
        (Ast.Unary
           ( Token.of_token_kind ~kind:Token_kind.Minus
           , Ast.Literal (Ast.String "42.") ))
    |> Result.is_error ) ;
  check_value
    (evaluate
       (Ast.Unary
          ( Token.of_token_kind ~kind:Token_kind.Bang
          , Ast.Literal (Ast.Number 42.) )))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Unary
          ( Token.of_token_kind ~kind:Token_kind.Bang
          , Ast.Literal (Ast.Bool false) )))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Unary
          (Token.of_token_kind ~kind:Token_kind.Bang, Ast.Literal Ast.Nil)))
    (Ok (Value.Bool true)) ;
  check_true
    ( evaluate
        (Ast.Unary
           ( Token.of_token_kind ~kind:Token_kind.Plus
           , Ast.Literal (Ast.Number 42.) ))
    |> Result.is_error )

let evaluate_binary () =
  let n1 = 2. in
  let n2 = 1. in
  let n1_ast = Ast.Literal (Ast.Number n1) in
  let n2_ast = Ast.Literal (Ast.Number n2) in
  check_value
    (evaluate
       (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Plus, n2_ast)))
    (Ok (Value.Number (n1 +. n2))) ;
  check_value
    (evaluate
       (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Minus, n2_ast)))
    (Ok (Value.Number (n1 -. n2))) ;
  check_value
    (evaluate
       (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Star, n2_ast)))
    (Ok (Value.Number (n1 *. n2))) ;
  check_value
    (evaluate
       (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Slash, n2_ast)))
    (Ok (Value.Number (n1 /. n2))) ;
  check_true
    ( evaluate
        (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Bang, n2_ast))
    |> Result.is_error ) ;
  let str1 = "hello" in
  let str2 = "world" in
  let str1_ast = Ast.Literal (Ast.String str1) in
  let str2_ast = Ast.Literal (Ast.String str2) in
  check_value
    (evaluate
       (Ast.Binary
          (str1_ast, Token.of_token_kind ~kind:Token_kind.Plus, str2_ast)))
    (Ok (Value.String (str1 ^ str2))) ;
  check_true
    ( evaluate
        (Ast.Binary
           (str1_ast, Token.of_token_kind ~kind:Token_kind.Bang, str2_ast))
    |> Result.is_error ) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Greater, n2_ast)))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Greater_equal, n2_ast)))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Less, n2_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Less_equal, n2_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, n1_ast)))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          (str1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, str1_ast)))
    (Ok (Value.Bool true)) ;
  let t_ast = Ast.Literal (Ast.Bool true) in
  check_value
    (evaluate
       (Ast.Binary
          (t_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, t_ast)))
    (Ok (Value.Bool true)) ;
  let f_ast = Ast.Literal (Ast.Bool false) in
  check_value
    (evaluate
       (Ast.Binary
          (f_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, f_ast)))
    (Ok (Value.Bool true)) ;
  let nil_ast = Ast.Literal Ast.Nil in
  check_value
    (evaluate
       (Ast.Binary
          (nil_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast)))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, str1_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, t_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (str1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, t_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (str1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (t_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast)))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          (n1_ast, Token.of_token_kind ~kind:Token_kind.Bang_equal, n2_ast)))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          (str1_ast, Token.of_token_kind ~kind:Token_kind.Bang_equal, str2_ast)))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          (t_ast, Token.of_token_kind ~kind:Token_kind.Bang_equal, f_ast)))
    (Ok (Value.Bool true)) ;
  let bad_expression =
    Ast.Unary
      ( Token.of_token_kind ~kind:Token_kind.Minus
      , Ast.Literal (Ast.String "42.") )
  in
  check_true
    ( evaluate
        (Ast.Binary
           (bad_expression, Token.of_token_kind ~kind:Token_kind.Plus, n2_ast))
    |> Result.is_error ) ;
  check_true
    ( evaluate
        (Ast.Binary
           (n1_ast, Token.of_token_kind ~kind:Token_kind.Plus, bad_expression))
    |> Result.is_error ) ;
  check_true
    ( evaluate
        (Ast.Binary
           ( bad_expression
           , Token.of_token_kind ~kind:Token_kind.Plus
           , bad_expression ))
    |> Result.is_error )

let all =
  [ Alcotest.test_case "Evaluate literal" `Quick evaluate_literal
  ; Alcotest.test_case "Evaluate grouping" `Quick evaluate_grouping
  ; Alcotest.test_case "Evaluate unary" `Quick evaluate_unary
  ; Alcotest.test_case "Evaluate binary" `Quick evaluate_binary ]
