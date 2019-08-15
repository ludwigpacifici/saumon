open Saumon
module A = Alcotest_testable

let check =
  Alcotest.(check (result (list A.token) (list A.scanner_error)))
    "check Scanner.scan_tokens"

let scan_empty () =
  check (Scanner.scan_tokens "")
    (Ok
       [ Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:0) ])

let scan_comment () =
  check (Scanner.scan_tokens "//")
    (Ok
       [ Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:0) ])

let scan_letf_paren () =
  check (Scanner.scan_tokens "(")
    (Ok
       [ Token.make ~kind:Token_kind.Left_paren
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_right_paren () =
  check (Scanner.scan_tokens ")")
    (Ok
       [ Token.make ~kind:Token_kind.Right_paren
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_left_brace () =
  check (Scanner.scan_tokens "{")
    (Ok
       [ Token.make ~kind:Token_kind.Left_brace
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_rigth_brace () =
  check (Scanner.scan_tokens "}")
    (Ok
       [ Token.make ~kind:Token_kind.Right_brace
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_coma () =
  check (Scanner.scan_tokens ",")
    (Ok
       [ Token.make ~kind:Token_kind.Comma
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_dot () =
  check (Scanner.scan_tokens ".")
    (Ok
       [ Token.make ~kind:Token_kind.Dot
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_minus () =
  check (Scanner.scan_tokens "-")
    (Ok
       [ Token.make ~kind:Token_kind.Minus
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_plus () =
  check (Scanner.scan_tokens "+")
    (Ok
       [ Token.make ~kind:Token_kind.Plus
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_semicolon () =
  check (Scanner.scan_tokens ";")
    (Ok
       [ Token.make ~kind:Token_kind.Semicolon
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_slash () =
  check (Scanner.scan_tokens "/")
    (Ok
       [ Token.make ~kind:Token_kind.Slash
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_star () =
  check (Scanner.scan_tokens "*")
    (Ok
       [ Token.make ~kind:Token_kind.Star
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_bang_equal () =
  check (Scanner.scan_tokens "!=")
    (Ok
       [ Token.make ~kind:Token_kind.Bang_equal
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_bang () =
  check (Scanner.scan_tokens "!")
    (Ok
       [ Token.make ~kind:Token_kind.Bang
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_equal_equal () =
  check (Scanner.scan_tokens "==")
    (Ok
       [ Token.make ~kind:Token_kind.Equal_equal
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_equal () =
  check (Scanner.scan_tokens "=")
    (Ok
       [ Token.make ~kind:Token_kind.Equal
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_greater_equal () =
  check (Scanner.scan_tokens "<=")
    (Ok
       [ Token.make ~kind:Token_kind.Greater_equal
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_greater () =
  check (Scanner.scan_tokens "<")
    (Ok
       [ Token.make ~kind:Token_kind.Greater
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_less_equal () =
  check (Scanner.scan_tokens ">=")
    (Ok
       [ Token.make ~kind:Token_kind.Less_equal
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_less () =
  check (Scanner.scan_tokens ">")
    (Ok
       [ Token.make ~kind:Token_kind.Less
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_and () =
  check
    (Scanner.scan_tokens "and")
    (Ok
       [ Token.make ~kind:Token_kind.And
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:3) ])

let scan_class () =
  check
    (Scanner.scan_tokens "class")
    (Ok
       [ Token.make ~kind:Token_kind.Class
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:5) ])

let scan_else () =
  check
    (Scanner.scan_tokens "else")
    (Ok
       [ Token.make ~kind:Token_kind.Else
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:4) ])

let scan_fun () =
  check
    (Scanner.scan_tokens "fun")
    (Ok
       [ Token.make ~kind:Token_kind.Fun
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:3) ])

let scan_for () =
  check
    (Scanner.scan_tokens "for")
    (Ok
       [ Token.make ~kind:Token_kind.For
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:3) ])

let scan_if () =
  check (Scanner.scan_tokens "if")
    (Ok
       [ Token.make ~kind:Token_kind.If
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_nil () =
  check
    (Scanner.scan_tokens "nil")
    (Ok
       [ Token.make ~kind:Token_kind.Nil
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:3) ])

let scan_or () =
  check (Scanner.scan_tokens "or")
    (Ok
       [ Token.make ~kind:Token_kind.Or
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_print () =
  check
    (Scanner.scan_tokens "print")
    (Ok
       [ Token.make ~kind:Token_kind.Print
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:5) ])

let scan_return () =
  check
    (Scanner.scan_tokens "return")
    (Ok
       [ Token.make ~kind:Token_kind.Return
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:6) ])

let scan_super () =
  check
    (Scanner.scan_tokens "super")
    (Ok
       [ Token.make ~kind:Token_kind.Super
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:5) ])

let scan_this () =
  check
    (Scanner.scan_tokens "this")
    (Ok
       [ Token.make ~kind:Token_kind.This
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:4) ])

let scan_true () =
  check
    (Scanner.scan_tokens "true")
    (Ok
       [ Token.make ~kind:Token_kind.True
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:4) ])

let scan_var () =
  check
    (Scanner.scan_tokens "var")
    (Ok
       [ Token.make ~kind:Token_kind.Var
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:3) ])

let scan_while () =
  check
    (Scanner.scan_tokens "while")
    (Ok
       [ Token.make ~kind:Token_kind.While
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:5) ])

let scan_newline () =
  check (Scanner.scan_tokens "\n")
    (Ok
       [ Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:2 ~column:0) ])

let scan_space () =
  check (Scanner.scan_tokens " ")
    (Ok
       [ Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:1) ])

let scan_identifier () =
  check
    (Scanner.scan_tokens "ocaml")
    (Ok
       [ Token.make ~kind:(Token_kind.Identifier "ocaml")
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:5) ])

let scan_string () =
  check
    (Scanner.scan_tokens "\"\"")
    (Ok
       [ Token.make ~kind:(Token_kind.String "")
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ]) ;
  check
    (Scanner.scan_tokens "\"rust\"")
    (Ok
       [ Token.make ~kind:(Token_kind.String "rust")
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:6) ])

let scan_numder () =
  check
    (Scanner.scan_tokens "42.0")
    (Ok
       [ Token.make ~kind:(Token_kind.Number 42.)
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:4) ]) ;
  check
    (Scanner.scan_tokens "42.")
    (Ok
       [ Token.make ~kind:(Token_kind.Number 42.)
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:3) ]) ;
  check (Scanner.scan_tokens "42")
    (Ok
       [ Token.make ~kind:(Token_kind.Number 42.)
           ~location:(Location.make ~line:1 ~column:0)
       ; Token.make ~kind:Token_kind.Eof
           ~location:(Location.make ~line:1 ~column:2) ])

let scan_error () =
  check (Scanner.scan_tokens "``")
    (Error
       [ { location = Location.make ~line:1 ~column:0
         ; where = "`"
         ; message = "Unknown input" }
       ; { location = Location.make ~line:1 ~column:1
         ; where = "`"
         ; message = "Unknown input" } ])

let all =
  [ Alcotest.test_case "Scan empty" `Quick scan_empty
  ; Alcotest.test_case "Scan comment" `Quick scan_comment
  ; Alcotest.test_case "Scan letf paren" `Quick scan_letf_paren
  ; Alcotest.test_case "Scan right paren" `Quick scan_right_paren
  ; Alcotest.test_case "Scan left brace" `Quick scan_left_brace
  ; Alcotest.test_case "Scan rigth brace" `Quick scan_rigth_brace
  ; Alcotest.test_case "Scan coma" `Quick scan_coma
  ; Alcotest.test_case "Scan dot" `Quick scan_dot
  ; Alcotest.test_case "Scan minus" `Quick scan_minus
  ; Alcotest.test_case "Scan plus" `Quick scan_plus
  ; Alcotest.test_case "Scan semicolon" `Quick scan_semicolon
  ; Alcotest.test_case "Scan slash" `Quick scan_slash
  ; Alcotest.test_case "Scan star" `Quick scan_star
  ; Alcotest.test_case "Scan bang equal" `Quick scan_bang_equal
  ; Alcotest.test_case "Scan bang" `Quick scan_bang
  ; Alcotest.test_case "Scan equal equal" `Quick scan_equal_equal
  ; Alcotest.test_case "Scan equal" `Quick scan_equal
  ; Alcotest.test_case "Scan greater equal" `Quick scan_greater_equal
  ; Alcotest.test_case "Scan greater" `Quick scan_greater
  ; Alcotest.test_case "Scan less equal" `Quick scan_less_equal
  ; Alcotest.test_case "Scan less" `Quick scan_less
  ; Alcotest.test_case "Scan and" `Quick scan_and
  ; Alcotest.test_case "Scan class" `Quick scan_class
  ; Alcotest.test_case "Scan else" `Quick scan_else
  ; Alcotest.test_case "Scan fun" `Quick scan_fun
  ; Alcotest.test_case "Scan for" `Quick scan_for
  ; Alcotest.test_case "Scan if" `Quick scan_if
  ; Alcotest.test_case "Scan nil" `Quick scan_nil
  ; Alcotest.test_case "Scan or" `Quick scan_or
  ; Alcotest.test_case "Scan print" `Quick scan_print
  ; Alcotest.test_case "Scan return" `Quick scan_return
  ; Alcotest.test_case "Scan super" `Quick scan_super
  ; Alcotest.test_case "Scan this" `Quick scan_this
  ; Alcotest.test_case "Scan true" `Quick scan_true
  ; Alcotest.test_case "Scan var" `Quick scan_var
  ; Alcotest.test_case "Scan while" `Quick scan_while
  ; Alcotest.test_case "Scan newline" `Quick scan_newline
  ; Alcotest.test_case "Scan space" `Quick scan_space
  ; Alcotest.test_case "Scan identifier" `Quick scan_identifier
  ; Alcotest.test_case "Scan string" `Quick scan_string
  ; Alcotest.test_case "Scan numder" `Quick scan_numder
  ; Alcotest.test_case "Scan error" `Quick scan_error ]
