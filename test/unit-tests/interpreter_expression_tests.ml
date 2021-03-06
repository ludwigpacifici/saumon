open Base
open Saumon
open Omnipresent
module A = Alcotest_testable

let check_value =
  Alcotest.(check (result A.value A.value_error)) "check Interpreter.evaluate"

let check_true =
  Alcotest.(check bool) "check Interpreter.evaluate is error" true

let evaluate = Interpreter.evaluate (Environment.empty ()) >> Result.map ~f:snd

let evaluate_literal () =
  check_value (evaluate (Ast.Literal (Ast.Number 42.))) (Ok (Value.Number 42.)) ;
  check_value
    (evaluate (Ast.Literal (Ast.String "hello")))
    (Ok (Value.String "hello")) ;
  check_value (evaluate (Ast.Literal (Ast.Bool true))) (Ok (Value.Bool true)) ;
  check_value (evaluate (Ast.Literal (Ast.Bool false))) (Ok (Value.Bool false)) ;
  check_value (evaluate (Ast.Literal Ast.Nil)) (Ok Value.Nil)

let evaluate_grouping () =
  let left_paren = Token.of_token_kind ~kind:Token_kind.Left_paren in
  let right_paren = Token.of_token_kind ~kind:Token_kind.Right_paren in
  check_value
    (evaluate
       (Ast.Grouping
          {left_paren; expr= Ast.Literal (Ast.Number 42.); right_paren}))
    (Ok (Value.Number 42.))

let evaluate_unary () =
  check_value
    (evaluate
       (Ast.Unary
          { operator= Token.of_token_kind ~kind:Token_kind.Minus
          ; expr= Ast.Literal (Ast.Number 42.) }))
    (Ok (Value.Number (-42.))) ;
  check_true
    ( evaluate
        (Ast.Unary
           { operator= Token.of_token_kind ~kind:Token_kind.Minus
           ; expr= Ast.Literal (Ast.String "42.") })
    |> Result.is_error ) ;
  check_value
    (evaluate
       (Ast.Unary
          { operator= Token.of_token_kind ~kind:Token_kind.Bang
          ; expr= Ast.Literal (Ast.Number 42.) }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Unary
          { operator= Token.of_token_kind ~kind:Token_kind.Bang
          ; expr= Ast.Literal (Ast.Bool false) }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Unary
          { operator= Token.of_token_kind ~kind:Token_kind.Bang
          ; expr= Ast.Literal Ast.Nil }))
    (Ok (Value.Bool true)) ;
  check_true
    ( evaluate
        (Ast.Unary
           { operator= Token.of_token_kind ~kind:Token_kind.Plus
           ; expr= Ast.Literal (Ast.Number 42.) })
    |> Result.is_error )

let evaluate_binary () =
  let n1 = 1. in
  let n2 = 2. in
  let n1_ast = Ast.Literal (Ast.Number n1) in
  let n2_ast = Ast.Literal (Ast.Number n2) in
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Plus
          ; right_expr= n2_ast }))
    (Ok (Value.Number (n1 +. n2))) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Minus
          ; right_expr= n2_ast }))
    (Ok (Value.Number (n1 -. n2))) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Star
          ; right_expr= n2_ast }))
    (Ok (Value.Number (n1 *. n2))) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Slash
          ; right_expr= n2_ast }))
    (Ok (Value.Number (n1 /. n2))) ;
  check_true
    ( evaluate
        (Ast.Binary
           { left_expr= n1_ast
           ; operator= Token.of_token_kind ~kind:Token_kind.Bang
           ; right_expr= n2_ast })
    |> Result.is_error ) ;
  let str1 = "hello" in
  let str2 = "world" in
  let str1_ast = Ast.Literal (Ast.String str1) in
  let str2_ast = Ast.Literal (Ast.String str2) in
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= str1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Plus
          ; right_expr= str2_ast }))
    (Ok (Value.String (str1 ^ str2))) ;
  check_true
    ( evaluate
        (Ast.Binary
           { left_expr= str1_ast
           ; operator= Token.of_token_kind ~kind:Token_kind.Bang
           ; right_expr= str2_ast })
    |> Result.is_error ) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Less
          ; right_expr= n2_ast }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Less_equal
          ; right_expr= n2_ast }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Greater
          ; right_expr= n2_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Greater_equal
          ; right_expr= n2_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= n1_ast }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= str1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= str1_ast }))
    (Ok (Value.Bool true)) ;
  let t_ast = Ast.Literal (Ast.Bool true) in
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= t_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= t_ast }))
    (Ok (Value.Bool true)) ;
  let f_ast = Ast.Literal (Ast.Bool false) in
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= f_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= f_ast }))
    (Ok (Value.Bool true)) ;
  let nil_ast = Ast.Literal Ast.Nil in
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= nil_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= nil_ast }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= str1_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= t_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= nil_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= str1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= t_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= str1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= nil_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= t_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Equal_equal
          ; right_expr= nil_ast }))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= n1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Bang_equal
          ; right_expr= n2_ast }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= str1_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Bang_equal
          ; right_expr= str2_ast }))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          { left_expr= t_ast
          ; operator= Token.of_token_kind ~kind:Token_kind.Bang_equal
          ; right_expr= f_ast }))
    (Ok (Value.Bool true)) ;
  let bad_expression =
    Ast.Unary
      { operator= Token.of_token_kind ~kind:Token_kind.Minus
      ; expr= Ast.Literal (Ast.String "42.") } in
  check_true
    ( evaluate
        (Ast.Binary
           { left_expr= bad_expression
           ; operator= Token.of_token_kind ~kind:Token_kind.Plus
           ; right_expr= n2_ast })
    |> Result.is_error ) ;
  check_true
    ( evaluate
        (Ast.Binary
           { left_expr= n1_ast
           ; operator= Token.of_token_kind ~kind:Token_kind.Plus
           ; right_expr= bad_expression })
    |> Result.is_error ) ;
  check_true
    ( evaluate
        (Ast.Binary
           { left_expr= bad_expression
           ; operator= Token.of_token_kind ~kind:Token_kind.Plus
           ; right_expr= bad_expression })
    |> Result.is_error )

let evaluate_conditional () =
  let true_ast = Ast.Literal (Ast.Bool true) in
  let false_ast = Ast.Literal (Ast.Bool false) in
  let and_token = Token.of_token_kind ~kind:Token_kind.And in
  let or_token = Token.of_token_kind ~kind:Token_kind.Or in
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= true_ast; operator= and_token; right_expr= true_ast}))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= false_ast; operator= and_token; right_expr= true_ast}))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= true_ast; operator= and_token; right_expr= false_ast}))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= false_ast; operator= and_token; right_expr= false_ast}))
    (Ok (Value.Bool false)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= true_ast; operator= or_token; right_expr= true_ast}))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= false_ast; operator= or_token; right_expr= true_ast}))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= true_ast; operator= or_token; right_expr= false_ast}))
    (Ok (Value.Bool true)) ;
  check_value
    (evaluate
       (Ast.Binary
          {left_expr= false_ast; operator= or_token; right_expr= false_ast}))
    (Ok (Value.Bool false)) ;
  (* Truethiness checked works also with not boolean values *)
  let nil = Ast.Literal Ast.Nil in
  let number_raw = 42. in
  let number_value = Value.Number 42. in
  let number = Ast.Literal (Ast.Number number_raw) in
  check_value
    (evaluate
       (Ast.Binary {left_expr= number; operator= and_token; right_expr= number}))
    (Ok number_value) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= nil; operator= and_token; right_expr= number}))
    (Ok Value.Nil) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= number; operator= and_token; right_expr= nil}))
    (Ok Value.Nil) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= nil; operator= and_token; right_expr= nil}))
    (Ok Value.Nil) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= number; operator= or_token; right_expr= number}))
    (Ok number_value) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= nil; operator= or_token; right_expr= number}))
    (Ok number_value) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= number; operator= or_token; right_expr= nil}))
    (Ok number_value) ;
  check_value
    (evaluate
       (Ast.Binary {left_expr= nil; operator= or_token; right_expr= nil}))
    (Ok Value.Nil)

let all =
  [ Alcotest.test_case "Evaluate literal" `Quick evaluate_literal
  ; Alcotest.test_case "Evaluate grouping" `Quick evaluate_grouping
  ; Alcotest.test_case "Evaluate unary" `Quick evaluate_unary
  ; Alcotest.test_case "Evaluate binary" `Quick evaluate_binary
  ; Alcotest.test_case "Evaluate conditional" `Quick evaluate_conditional ]
