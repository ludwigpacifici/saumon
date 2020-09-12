open Base
open Saumon
module Ou = Alcohoul

let empty_block () =
  Ou.make_program_exn "{}"
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_true (List.is_empty vs))
  |> Result.is_ok |> Ou.check_true

let block_with_several_statements () =
  let first_value = 1. in
  let second_value = 2. in
  (* { print 1.; print 2.;} *)
  let code =
    "{ print "
    ^ Float.to_string first_value
    ^ "; print "
    ^ Float.to_string second_value
    ^ "; }" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_true (Environment.is_empty env) ;
         Ou.check_value_list vs
           [Value.Number first_value; Value.Number second_value])
  |> Result.is_ok |> Ou.check_true

let variable_declaration_shadows_with_block () =
  let id = "x" in
  let first_value = 1. in
  let second_value = 2. in
  (* var x = 1.; { print x; var x = 2.; print x;} *)
  let code =
    " var " ^ id ^ " = "
    ^ Float.to_string first_value
    ^ "; { print " ^ id ^ "; var " ^ id ^ " = "
    ^ Float.to_string second_value
    ^ "; print " ^ id ^ "; }" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id)
           (Some (Value.Number first_value)) ;
         Ou.check_value_list vs
           [Value.Number first_value; Value.Number second_value])
  |> Result.is_ok |> Ou.check_true

let variable_assignment_persist_with_block () =
  let id = "x" in
  let first_value = 1. in
  let second_value = 2. in
  (* var x = 1.; { print x; x = 2.; print x;} print x; *)
  let code =
    " var " ^ id ^ " = "
    ^ Float.to_string first_value
    ^ "; { print " ^ id ^ "; " ^ id ^ " = "
    ^ Float.to_string second_value
    ^ "; print " ^ id ^ "; } print " ^ id ^ "; " in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id)
           (Some (Value.Number second_value)) ;
         Ou.check_value_list vs
           [ Value.Number first_value; Value.Number second_value
           ; Value.Number second_value ])
  |> Result.is_ok |> Ou.check_true

let all =
  [ Alcotest.test_case "Empty block" `Quick empty_block
  ; Alcotest.test_case "Block with several statements" `Quick
      block_with_several_statements
  ; Alcotest.test_case "Variable declaration shadows with block" `Quick
      variable_declaration_shadows_with_block
  ; Alcotest.test_case "Variable assignment persist with block" `Quick
      variable_assignment_persist_with_block ]
