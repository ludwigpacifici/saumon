open Base
open Saumon
module Ou = Alcohoul

let print_number () =
  let value = 42. in
  let code = (* print 42.; *) "print " ^ Float.to_string value ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Number value])
  |> Result.is_ok |> Ou.check_true

let print_string () =
  let value = "hello" in
  let code = (* print "hello"; *) "print \"" ^ value ^ "\";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.String value])
  |> Result.is_ok |> Ou.check_true

let print_bool value () =
  let code = (* print true/false; *) "print " ^ Bool.to_string value ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Bool value])
  |> Result.is_ok |> Ou.check_true

let print_nil () =
  Ou.make_program_exn "print nil;"
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Nil])
  |> Result.is_ok |> Ou.check_true

let print_expression () =
  Ou.make_program_exn "print 2 + 2;"
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Number 4.])
  |> Result.is_ok |> Ou.check_true

let discard_expression () =
  Ou.make_program_exn "2;"
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [])
  |> Result.is_ok |> Ou.check_true

let bad_expression () =
  Ou.make_program_exn "2 + nil;"
  |> Ou.execute_with_empty_env ~k:Ou.ignore2
  |> Result.is_error |> Ou.check_true

let bad_print () =
  Ou.make_program_exn "print 2 + nil;"
  |> Ou.execute_with_empty_env ~k:Ou.ignore2
  |> Result.is_error |> Ou.check_true

let declare_variable_without_initialization () =
  let id = "x" in
  let code = (* var x; *) "var " ^ id ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id) (Some Value.Nil) ;
         Ou.check_value_list vs [])
  |> Result.is_ok |> Ou.check_true

let declare_variable_with_initialization () =
  let id = "x" in
  let value = 42. in
  let code =
    (* var x = 42.; *) "var " ^ id ^ " = " ^ Float.to_string value ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id)
           (Some (Value.Number value)) ;
         Ou.check_value_list vs [])
  |> Result.is_ok |> Ou.check_true

let declare_variable_with_expression () =
  let id = "x" in
  let value = 42. in
  let code =
    (* var x = 42. + 42.; *)
    "var " ^ id ^ " = " ^ Float.to_string value ^ " + " ^ Float.to_string value
    ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id)
           (Some (Value.Number (value +. value))) ;
         Ou.check_value_list vs [])
  |> Result.is_ok |> Ou.check_true

let bad_initialized_variable_declaration () =
  Ou.make_program_exn "var x = 2 + nil;"
  |> Ou.execute_with_empty_env ~k:Ou.ignore2
  |> Result.is_error |> Ou.check_true

let declare_variable_and_print () =
  let id = "x" in
  let value = 42. in
  let code =
    (* var x = 42.; print x; *)
    "var " ^ id ^ " = " ^ Float.to_string value ^ "; print " ^ id ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         let v = Value.Number value in
         Ou.check_option_value (Environment.get ~env ~id) (Some v) ;
         Ou.check_value_list vs [v])
  |> Result.is_ok |> Ou.check_true

let declare_variable_and_shadow () =
  let id = "x" in
  let value = 42. in
  let code =
    (* var x = 41; var x = 42.; *)
    "var " ^ id ^ " = 41; var " ^ id ^ " = " ^ Float.to_string value ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id)
           (Some (Value.Number value)) ;
         Ou.check_value_list vs [])
  |> Result.is_ok |> Ou.check_true

let declare_variable_and_assign () =
  let id = "x" in
  let value = 42. in
  let code =
    (* var x = 43.; x = 42.; print x;*)
    "var " ^ id ^ " = "
    ^ Float.to_string (value +. 1.)
    ^ "; " ^ id ^ " = " ^ Float.to_string value ^ "; print " ^ id ^ ";" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         let v = Value.Number value in
         Ou.check_option_value (Environment.get ~env ~id) (Some v) ;
         Ou.check_value_list vs [v])
  |> Result.is_ok |> Ou.check_true

let initialized_variable_without_declaration () =
  Ou.make_program_exn "x = 2;"
  |> Ou.execute_with_empty_env ~k:Ou.ignore2
  |> Result.is_error |> Ou.check_true

let all =
  [ Alcotest.test_case "Print number" `Quick print_number
  ; Alcotest.test_case "Print string" `Quick print_string
  ; Alcotest.test_case "Print false" `Quick (print_bool false)
  ; Alcotest.test_case "Print true" `Quick (print_bool true)
  ; Alcotest.test_case "Print nil" `Quick print_nil
  ; Alcotest.test_case "Print expression" `Quick print_expression
  ; Alcotest.test_case "Discard expression" `Quick discard_expression
  ; Alcotest.test_case "Bad expression" `Quick bad_expression
  ; Alcotest.test_case "Bad print" `Quick bad_print
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
