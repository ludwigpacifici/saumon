open Base
open Saumon
module Ou = Alcohoul

let if_branch_with_true_condition () =
  let first_value = 1. in
  (* if ( true ) print 1.; *)
  let code = "if ( true ) print " ^ Float.to_string first_value ^ "; " in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Number first_value])
  |> Result.is_ok
  |> Ou.check_true

let if_branch_with_false_condition () =
  let first_value = 1. in
  (* if ( false ) print 1.; *)
  let code = "if ( false ) print " ^ Float.to_string first_value ^ "; " in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [])
  |> Result.is_ok
  |> Ou.check_true

let if_else_branch_with_true_condition () =
  let first_value = 1. in
  let second_value = 2. in
  (* if ( true ) print 1.; else print 2.; *)
  let code =
    "if ( true ) print "
    ^ Float.to_string first_value
    ^ "; else print "
    ^ Float.to_string second_value
    ^ ";"
  in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Number first_value])
  |> Result.is_ok
  |> Ou.check_true

let if_else_branch_with_false_condition () =
  let first_value = 1. in
  let second_value = 2. in
  (* if ( false ) print 1.; else print 2.; *)
  let code =
    "if ( false ) print "
    ^ Float.to_string first_value
    ^ "; else print "
    ^ Float.to_string second_value
    ^ ";"
  in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs [Value.Number second_value])
  |> Result.is_ok
  |> Ou.check_true

let if_consume_one_instruction () =
  let first_value = 1. in
  (* if ( true ) print 1.; print 1.;*)
  let code =
    "if ( true ) print "
    ^ Float.to_string first_value
    ^ "; print "
    ^ Float.to_string first_value
    ^ "; "
  in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs
           [Value.Number first_value; Value.Number first_value])
  |> Result.is_ok
  |> Ou.check_true

let else_consume_one_instruction () =
  let first_value = 1. in
  let second_value = 2. in
  (* if ( false ) print 1.; else print 2.; print 2.;*)
  let code =
    "if ( false ) print "
    ^ Float.to_string first_value
    ^ "; else print "
    ^ Float.to_string second_value
    ^ "; print "
    ^ Float.to_string second_value
    ^ "; "
  in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs
           [Value.Number second_value; Value.Number second_value])
  |> Result.is_ok
  |> Ou.check_true

let all =
  [ Alcotest.test_case "If branch with true condition" `Quick
      if_branch_with_true_condition
  ; Alcotest.test_case "If branch with false condition" `Quick
      if_branch_with_false_condition
  ; Alcotest.test_case "If else branch with true conditiona" `Quick
      if_else_branch_with_true_condition
  ; Alcotest.test_case "If else branch with false conditiona" `Quick
      if_else_branch_with_false_condition
  ; Alcotest.test_case "If consume one instruction" `Quick
      if_consume_one_instruction
  ; Alcotest.test_case "Else consume one instruction" `Quick
      else_consume_one_instruction ]
