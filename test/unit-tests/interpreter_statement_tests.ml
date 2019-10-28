open Base
open Saumon
module A = Alcotest_testable

let check_value_list =
  Alcotest.(check (list A.value)) "check Interpreter.execute_k"

let check_option_value =
  Alcotest.(check (option A.value)) "check Interpreter.execute_k"

let check_true = Alcotest.(check bool) "check Interpreter.execute_k" true

(* Generate the ast from a string of Lox code. Useful for brief and
   comprehensives tests. Raise `invalid_arg` either when the scanner or the
   parser fails. *)
let make_program_exn (str : string) : Ast.Program.t =
  Scanner.scan_tokens str
  |> Caml.Result.get_ok
  |> Parser.parse
  |> Caml.Result.get_ok

(* Execute a program always with an empty environment. Useful for brief and
   comprehensives tests. *)
let execute_with_empty_env
    ~(k : Environment.t -> Value.t list -> unit)
    (p : Ast.Program.t) =
  let env = Environment.empty () in
  Interpreter.execute_k ~k env p

(* Ignore for function with two parameters. *)
let ignore2 _ = ignore

let print_number () =
  let value = 42. in
  make_program_exn ("print " ^ Float.to_string value ^ ";")
  |> execute_with_empty_env ~k:(fun env vs ->
         check_true (Environment.is_empty env) ;
         check_value_list vs [Value.Number value])
  |> Result.is_ok
  |> check_true

let print_string () =
  let value = "hello" in
  make_program_exn ("print \"" ^ value ^ "\";")
  |> execute_with_empty_env ~k:(fun env vs ->
         check_true (Environment.is_empty env) ;
         check_value_list vs [Value.String value])
  |> Result.is_ok
  |> check_true

let print_bool value () =
  make_program_exn ("print " ^ Bool.to_string value ^ ";")
  |> execute_with_empty_env ~k:(fun env vs ->
         check_true (Environment.is_empty env) ;
         check_value_list vs [Value.Bool value])
  |> Result.is_ok
  |> check_true

let print_nil () =
  make_program_exn "print nil;"
  |> execute_with_empty_env ~k:(fun env vs ->
         check_true (Environment.is_empty env) ;
         check_value_list vs [Value.Nil])
  |> Result.is_ok
  |> check_true

let print_expression () =
  make_program_exn "print 2 + 2;"
  |> execute_with_empty_env ~k:(fun env vs ->
         check_true (Environment.is_empty env) ;
         check_value_list vs [Value.Number 4.])
  |> Result.is_ok
  |> check_true

let discard_expression () =
  make_program_exn "2;"
  |> execute_with_empty_env ~k:(fun env vs ->
         check_true (Environment.is_empty env) ;
         check_value_list vs [])
  |> Result.is_ok
  |> check_true

let bad_expression () =
  make_program_exn "2 + nil;"
  |> execute_with_empty_env ~k:ignore2
  |> Result.is_error
  |> check_true

let bad_print () =
  make_program_exn "print 2 + nil;"
  |> execute_with_empty_env ~k:ignore2
  |> Result.is_error
  |> check_true

let declare_variable_without_initialization () =
  let id = "x" in
  make_program_exn ("var " ^ id ^ ";")
  |> execute_with_empty_env ~k:(fun env vs ->
         check_option_value (Environment.get ~env ~id) (Some Value.Nil) ;
         check_value_list vs [])
  |> Result.is_ok
  |> check_true

let declare_variable_with_initialization () =
  let id = "x" in
  let value = 42. in
  make_program_exn ("var " ^ id ^ " = " ^ Float.to_string value ^ ";")
  |> execute_with_empty_env ~k:(fun env vs ->
         check_option_value (Environment.get ~env ~id)
           (Some (Value.Number value)) ;
         check_value_list vs [])
  |> Result.is_ok
  |> check_true

let declare_variable_with_expression () =
  let id = "x" in
  let value = 42. in
  make_program_exn
    ( "var "
    ^ id
    ^ " = "
    ^ Float.to_string value
    ^ " + "
    ^ Float.to_string value
    ^ ";" )
  |> execute_with_empty_env ~k:(fun env vs ->
         check_option_value (Environment.get ~env ~id)
           (Some (Value.Number (value +. value))) ;
         check_value_list vs [])
  |> Result.is_ok
  |> check_true

let bad_initialized_variable_declaration () =
  make_program_exn "var x = 2 + nil;"
  |> execute_with_empty_env ~k:ignore2
  |> Result.is_error
  |> check_true

let declare_variable_and_print () =
  let id = "x" in
  let value = 42. in
  make_program_exn
    ("var " ^ id ^ " = " ^ Float.to_string value ^ "; print " ^ id ^ ";")
  |> execute_with_empty_env ~k:(fun env vs ->
         let v = Value.Number value in
         check_option_value (Environment.get ~env ~id) (Some v) ;
         check_value_list vs [v])
  |> Result.is_ok
  |> check_true

let declare_variable_and_shadow () =
  let id = "x" in
  let value = 42. in
  make_program_exn
    ("var " ^ id ^ " = 41; var " ^ id ^ " = " ^ Float.to_string value ^ ";")
  |> execute_with_empty_env ~k:(fun env vs ->
         check_option_value (Environment.get ~env ~id)
           (Some (Value.Number value)) ;
         check_value_list vs [])
  |> Result.is_ok
  |> check_true

let declare_variable_and_assign () =
  let id = "x" in
  let value = 42. in
  make_program_exn
    ( "var "
    ^ id
    ^ " = "
    ^ Float.to_string (value +. 1.)
    ^ "; "
    ^ id
    ^ " = "
    ^ Float.to_string value
    ^ "; print "
    ^ id
    ^ ";" )
  |> execute_with_empty_env ~k:(fun env vs ->
         let v = Value.Number value in
         check_option_value (Environment.get ~env ~id) (Some v) ;
         check_value_list vs [v])
  |> Result.is_ok
  |> check_true

let initialized_variable_without_declaration () =
  make_program_exn "x = 2;"
  |> execute_with_empty_env ~k:ignore2
  |> Result.is_error
  |> check_true

let all =
  [ Alcotest.test_case "Print number" `Quick print_number
  ; Alcotest.test_case "Print string" `Quick print_string
  ; Alcotest.test_case "Print false" `Quick (print_bool false)
  ; Alcotest.test_case "Print true" `Quick (print_bool true)
  ; Alcotest.test_case "Print nil" `Quick print_nil
  ; Alcotest.test_case "Print expression" `Quick print_expression
  ; Alcotest.test_case "Discard expression" `Quick discard_expression
  ; Alcotest.test_case "Bad expression" `Quick bad_expression
  ; Alcotest.test_case "Bas print" `Quick bad_print
  ; Alcotest.test_case "Declare variable without initialization" `Quick
      declare_variable_without_initialization
  ; Alcotest.test_case "Declare variable with initialization" `Quick
      declare_variable_with_initialization
  ; Alcotest.test_case "Declare variable with expression" `Quick
      declare_variable_with_expression
  ; Alcotest.test_case "Bad initialized variable declaration" `Quick
      bad_initialized_variable_declaration
  ; Alcotest.test_case "Declare variable and print" `Quick
      declare_variable_and_print
  ; Alcotest.test_case "Declare variable and shadow" `Quick
      declare_variable_and_shadow
  ; Alcotest.test_case "Declare variable and assign" `Quick
      declare_variable_and_assign
  ; Alcotest.test_case "Initialized variable without declaration" `Quick
      initialized_variable_without_declaration ]
