open OUnit2
open Test_utils
open Saumon

let scan_empty () =
  Scanner.scan_tokens ""
  === ( [ Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:0) ]
      , false )

let scan_comment () =
  Scanner.scan_tokens "//"
  === ( [ Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:0) ]
      , false )

let scan_letf_paren () =
  Scanner.scan_tokens "("
  === ( [ Token.make ~kind:Token_kind.Left_paren
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_right_paren () =
  Scanner.scan_tokens ")"
  === ( [ Token.make ~kind:Token_kind.Right_paren
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_left_brace () =
  Scanner.scan_tokens "{"
  === ( [ Token.make ~kind:Token_kind.Left_brace
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_rigth_brace () =
  Scanner.scan_tokens "}"
  === ( [ Token.make ~kind:Token_kind.Right_brace
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_coma () =
  Scanner.scan_tokens ","
  === ( [ Token.make ~kind:Token_kind.Comma
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_dot () =
  Scanner.scan_tokens "."
  === ( [ Token.make ~kind:Token_kind.Dot
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_minus () =
  Scanner.scan_tokens "-"
  === ( [ Token.make ~kind:Token_kind.Minus
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_plus () =
  Scanner.scan_tokens "+"
  === ( [ Token.make ~kind:Token_kind.Plus
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_semicolon () =
  Scanner.scan_tokens ";"
  === ( [ Token.make ~kind:Token_kind.Semicolon
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_slash () =
  Scanner.scan_tokens "/"
  === ( [ Token.make ~kind:Token_kind.Slash
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_star () =
  Scanner.scan_tokens "*"
  === ( [ Token.make ~kind:Token_kind.Star
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_bang_equal () =
  Scanner.scan_tokens "!="
  === ( [ Token.make ~kind:Token_kind.Bang_equal
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_bang () =
  Scanner.scan_tokens "!"
  === ( [ Token.make ~kind:Token_kind.Bang
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_equal_equal () =
  Scanner.scan_tokens "=="
  === ( [ Token.make ~kind:Token_kind.Equal_equal
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_equal () =
  Scanner.scan_tokens "="
  === ( [ Token.make ~kind:Token_kind.Equal
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_greater_equal () =
  Scanner.scan_tokens "<="
  === ( [ Token.make ~kind:Token_kind.Greater_equal
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_greater () =
  Scanner.scan_tokens "<"
  === ( [ Token.make ~kind:Token_kind.Greater
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_less_equal () =
  Scanner.scan_tokens ">="
  === ( [ Token.make ~kind:Token_kind.Less_equal
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_less () =
  Scanner.scan_tokens ">"
  === ( [ Token.make ~kind:Token_kind.Less
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_and () =
  Scanner.scan_tokens "and"
  === ( [ Token.make ~kind:Token_kind.And
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:3) ]
      , false )

let scan_class () =
  Scanner.scan_tokens "class"
  === ( [ Token.make ~kind:Token_kind.Class
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:5) ]
      , false )

let scan_else () =
  Scanner.scan_tokens "else"
  === ( [ Token.make ~kind:Token_kind.Else
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:4) ]
      , false )

let scan_fun () =
  Scanner.scan_tokens "fun"
  === ( [ Token.make ~kind:Token_kind.Fun
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:3) ]
      , false )

let scan_for () =
  Scanner.scan_tokens "for"
  === ( [ Token.make ~kind:Token_kind.For
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:3) ]
      , false )

let scan_if () =
  Scanner.scan_tokens "if"
  === ( [ Token.make ~kind:Token_kind.If
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_nil () =
  Scanner.scan_tokens "nil"
  === ( [ Token.make ~kind:Token_kind.Nil
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:3) ]
      , false )

let scan_or () =
  Scanner.scan_tokens "or"
  === ( [ Token.make ~kind:Token_kind.Or
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_print () =
  Scanner.scan_tokens "print"
  === ( [ Token.make ~kind:Token_kind.Print
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:5) ]
      , false )

let scan_return () =
  Scanner.scan_tokens "return"
  === ( [ Token.make ~kind:Token_kind.Return
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:6) ]
      , false )

let scan_super () =
  Scanner.scan_tokens "super"
  === ( [ Token.make ~kind:Token_kind.Super
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:5) ]
      , false )

let scan_this () =
  Scanner.scan_tokens "this"
  === ( [ Token.make ~kind:Token_kind.This
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:4) ]
      , false )

let scan_true () =
  Scanner.scan_tokens "true"
  === ( [ Token.make ~kind:Token_kind.True
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:4) ]
      , false )

let scan_var () =
  Scanner.scan_tokens "var"
  === ( [ Token.make ~kind:Token_kind.Var
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:3) ]
      , false )

let scan_while () =
  Scanner.scan_tokens "while"
  === ( [ Token.make ~kind:Token_kind.While
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:5) ]
      , false )

let scan_newline () =
  Scanner.scan_tokens "\n"
  === ( [ Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:2 ~column:0) ]
      , false )

let scan_space () =
  Scanner.scan_tokens " "
  === ( [ Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , false )

let scan_identifier () =
  Scanner.scan_tokens "ocaml"
  === ( [ Token.make ~kind:(Token_kind.Identifier "ocaml")
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:5) ]
      , false )

let scan_string () =
  Scanner.scan_tokens "\"\""
  === ( [ Token.make ~kind:(Token_kind.String "")
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false ) ;
  Scanner.scan_tokens "\"rust\""
  === ( [ Token.make ~kind:(Token_kind.String "rust")
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:6) ]
      , false )

let scan_numder () =
  Scanner.scan_tokens "42.0"
  === ( [ Token.make ~kind:(Token_kind.Number 42.)
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:4) ]
      , false ) ;
  Scanner.scan_tokens "42."
  === ( [ Token.make ~kind:(Token_kind.Number 42.)
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:3) ]
      , false ) ;
  Scanner.scan_tokens "42"
  === ( [ Token.make ~kind:(Token_kind.Number 42.)
            ~location:(Location.make ~line:1 ~column:0)
        ; Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:2) ]
      , false )

let scan_error () =
  Scanner.scan_tokens "\\"
  === ( [ Token.make ~kind:Token_kind.Eof
            ~location:(Location.make ~line:1 ~column:1) ]
      , true )

let scanner_tests =
  [ ("Scan empty" >:: fun _ -> scan_empty ())
  ; ("Scan comment" >:: fun _ -> scan_comment ())
  ; ("Scan letf paren" >:: fun _ -> scan_letf_paren ())
  ; ("Scan right paren" >:: fun _ -> scan_right_paren ())
  ; ("Scan left brace" >:: fun _ -> scan_left_brace ())
  ; ("Scan rigth brace" >:: fun _ -> scan_rigth_brace ())
  ; ("Scan coma" >:: fun _ -> scan_coma ())
  ; ("Scan dot" >:: fun _ -> scan_dot ())
  ; ("Scan minus" >:: fun _ -> scan_minus ())
  ; ("Scan plus" >:: fun _ -> scan_plus ())
  ; ("Scan semicolon" >:: fun _ -> scan_semicolon ())
  ; ("Scan slash" >:: fun _ -> scan_slash ())
  ; ("Scan star" >:: fun _ -> scan_star ())
  ; ("Scan bang equal" >:: fun _ -> scan_bang_equal ())
  ; ("Scan bang" >:: fun _ -> scan_bang ())
  ; ("Scan equal equal" >:: fun _ -> scan_equal_equal ())
  ; ("Scan equal" >:: fun _ -> scan_equal ())
  ; ("Scan greater equal" >:: fun _ -> scan_greater_equal ())
  ; ("Scan greater" >:: fun _ -> scan_greater ())
  ; ("Scan less equal" >:: fun _ -> scan_less_equal ())
  ; ("Scan less" >:: fun _ -> scan_less ())
  ; ("Scan and" >:: fun _ -> scan_and ())
  ; ("Scan class" >:: fun _ -> scan_class ())
  ; ("Scan else" >:: fun _ -> scan_else ())
  ; ("Scan fun" >:: fun _ -> scan_fun ())
  ; ("Scan for" >:: fun _ -> scan_for ())
  ; ("Scan if" >:: fun _ -> scan_if ())
  ; ("Scan nil" >:: fun _ -> scan_nil ())
  ; ("Scan or" >:: fun _ -> scan_or ())
  ; ("Scan print" >:: fun _ -> scan_print ())
  ; ("Scan return" >:: fun _ -> scan_return ())
  ; ("Scan super" >:: fun _ -> scan_super ())
  ; ("Scan this" >:: fun _ -> scan_this ())
  ; ("Scan true" >:: fun _ -> scan_true ())
  ; ("Scan var" >:: fun _ -> scan_var ())
  ; ("Scan while" >:: fun _ -> scan_while ())
  ; ("Scan newline" >:: fun _ -> scan_newline ())
  ; ("Scan space" >:: fun _ -> scan_space ())
  ; ("Scan identifier" >:: fun _ -> scan_identifier ())
  ; ("Scan string" >:: fun _ -> scan_string ())
  ; ("Scan numder" >:: fun _ -> scan_numder ())
  ; ("Scan error" >:: fun _ -> scan_error ()) ]
