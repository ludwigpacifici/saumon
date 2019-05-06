open OUnit2
open Test_utils
open Saumon

let evaluate_literal () =
  Interpreter.evaluate (Ast.Literal (Ast.Number 42.)) === Ok (Value.Number 42.) ;
  Interpreter.evaluate (Ast.Literal (Ast.String "hello"))
  === Ok (Value.String "hello") ;
  Interpreter.evaluate (Ast.Literal (Ast.Bool true)) === Ok (Value.Bool true) ;
  Interpreter.evaluate (Ast.Literal (Ast.Bool false)) === Ok (Value.Bool false) ;
  Interpreter.evaluate (Ast.Literal Ast.Nil) === Ok Value.Nil

let evaluate_grouping () =
  let left = Token.of_token_kind ~kind:Token_kind.Left_paren in
  let right = Token.of_token_kind ~kind:Token_kind.Right_paren in
  Interpreter.evaluate
    (Ast.Grouping (left, Ast.Literal (Ast.Number 42.), right))
  === Ok (Value.Number 42.)

let evaluate_unary () =
  Interpreter.evaluate
    (Ast.Unary
       ( Token.of_token_kind ~kind:Token_kind.Minus
       , Ast.Literal (Ast.Number 42.) ))
  === Ok (Value.Number (-42.)) ;
  Interpreter.evaluate
    (Ast.Unary
       (Token.of_token_kind ~kind:Token_kind.Bang, Ast.Literal (Ast.Number 42.)))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Unary
       (Token.of_token_kind ~kind:Token_kind.Bang, Ast.Literal (Ast.Bool false)))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Unary (Token.of_token_kind ~kind:Token_kind.Bang, Ast.Literal Ast.Nil))
  === Ok (Value.Bool true)

let evaluate_binary () =
  let n1 = 2. in
  let n2 = 1. in
  let n1_ast = Ast.Literal (Ast.Number n1) in
  let n2_ast = Ast.Literal (Ast.Number n2) in
  Interpreter.evaluate
    (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Plus, n2_ast))
  === Ok (Value.Number (n1 +. n2)) ;
  Interpreter.evaluate
    (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Minus, n2_ast))
  === Ok (Value.Number (n1 -. n2)) ;
  Interpreter.evaluate
    (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Star, n2_ast))
  === Ok (Value.Number (n1 *. n2)) ;
  Interpreter.evaluate
    (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Slash, n2_ast))
  === Ok (Value.Number (n1 /. n2)) ;
  let str1 = "hello" in
  let str2 = "world" in
  let str1_ast = Ast.Literal (Ast.String str1) in
  let str2_ast = Ast.Literal (Ast.String str2) in
  Interpreter.evaluate
    (Ast.Binary (str1_ast, Token.of_token_kind ~kind:Token_kind.Plus, str2_ast))
  === Ok (Value.String (str1 ^ str2)) ;
  Interpreter.evaluate
    (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Greater, n2_ast))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Greater_equal, n2_ast))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Binary (n1_ast, Token.of_token_kind ~kind:Token_kind.Less, n2_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Less_equal, n2_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, n1_ast))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Binary
       (str1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, str1_ast))
  === Ok (Value.Bool true) ;
  let t_ast = Ast.Literal (Ast.Bool true) in
  Interpreter.evaluate
    (Ast.Binary (t_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, t_ast))
  === Ok (Value.Bool true) ;
  let f_ast = Ast.Literal (Ast.Bool false) in
  Interpreter.evaluate
    (Ast.Binary (f_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, f_ast))
  === Ok (Value.Bool true) ;
  let nil_ast = Ast.Literal Ast.Nil in
  Interpreter.evaluate
    (Ast.Binary
       (nil_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, str1_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, t_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (str1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, t_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (str1_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (t_ast, Token.of_token_kind ~kind:Token_kind.Equal_equal, nil_ast))
  === Ok (Value.Bool false) ;
  Interpreter.evaluate
    (Ast.Binary
       (n1_ast, Token.of_token_kind ~kind:Token_kind.Bang_equal, n2_ast))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Binary
       (str1_ast, Token.of_token_kind ~kind:Token_kind.Bang_equal, str2_ast))
  === Ok (Value.Bool true) ;
  Interpreter.evaluate
    (Ast.Binary (t_ast, Token.of_token_kind ~kind:Token_kind.Bang_equal, f_ast))
  === Ok (Value.Bool true)

let interpreter_tests =
  [ ("Evaluate literal" >:: fun _ -> evaluate_literal ())
  ; ("Evaluate grouping" >:: fun _ -> evaluate_grouping ())
  ; ("Evaluate unary" >:: fun _ -> evaluate_unary ())
  ; ("Evaluate binary" >:: fun _ -> evaluate_binary ()) ]
