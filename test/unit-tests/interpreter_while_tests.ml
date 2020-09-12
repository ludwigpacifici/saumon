open Base
open Saumon
module Ou = Alcohoul

let valid_while_loop () =
  let id = "i" in
  (* var i = 0; while ( i < 2 ) { print i; i = i + 1; } *)
  let code =
    "var " ^ id ^ " = 0; while ( " ^ id ^ " < 2 ) { print " ^ id ^ "; " ^ id
    ^ " = " ^ id ^ " + 1; }" in
  Ou.make_program_exn code
  |> Ou.execute_with_empty_env ~k:(fun env vs ->
         Ou.check_option_value (Environment.get ~env ~id)
           (Some (Value.Number 2.)) ;
         Ou.check_value_list vs [Value.Number 0.; Value.Number 1.])
  |> Result.is_ok |> Ou.check_true

let all = [Alcotest.test_case "Valid while loop" `Quick valid_while_loop]
