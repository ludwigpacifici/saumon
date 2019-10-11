open Base
open Saumon
module A = Alcotest_testable

let check_maybe_value = Alcotest.(check (option A.value)) "check Environment"

let check_bool = Alcotest.(check bool) "check bool Environment"

let environment_empty () =
  check_bool true (Environment.empty () |> Environment.is_empty)

let environment_with_empty_scopes () =
  Environment.empty ()
  |> (fun env -> Environment.push_scope ~env)
  |> Environment.is_empty |> check_bool true

let get_unknown_value_is_none () =
  Environment.empty ()
  |> (fun env -> Environment.get ~env ~id:"unknown")
  |> Option.is_none |> check_bool true

let pop_empty_environment_is_none () =
  Environment.empty ()
  |> (fun env -> Environment.pop_scope ~env)
  |> Option.is_none |> check_bool true

let define_and_get_value () =
  let id = "x" in
  let value = Value.Number 42. in
  Environment.empty ()
  |> (fun env -> Environment.define ~env ~id value)
  |> (fun env -> Environment.get ~env ~id)
  |> check_maybe_value (Some value)

let define_can_shadow () =
  let id = "x" in
  Environment.empty ()
  |> (fun env -> Environment.define ~env ~id Value.Nil)
  |> (fun env -> Environment.define ~env ~id (Value.Number 42.))
  |> (fun env -> Environment.get ~env ~id)
  |> check_maybe_value (Some (Value.Number 42.))

let define_can_shadow_in_new_scope () =
  let id = "x" in
  let env =
    Environment.empty ()
    |> (fun env -> Environment.define ~env ~id (Value.Bool true))
    |> (fun env -> Environment.push_scope ~env)
    |> fun env -> Environment.define ~env ~id (Value.Number 42.)
  in
  env
  |> (fun env -> Environment.get ~env ~id)
  |> check_maybe_value (Some (Value.Number 42.)) ;
  env
  |> (fun env -> Environment.pop_scope ~env)
  |> Option.map ~f:snd
  |> Option.bind ~f:(fun env -> Environment.get ~env ~id)
  |> check_maybe_value (Some (Value.Bool true))

let does_contains () =
  let id = "x" in
  Environment.empty ()
  |> (fun env -> Environment.define ~env ~id (Value.Number 42.))
  |> (fun env -> Environment.contains ~env ~id)
  |> check_bool true

let does_not_contains () =
  Environment.empty ()
  |> (fun env -> Environment.contains ~env ~id:"unknown")
  |> check_bool false

let declare_assign_with_same_scope () =
  let id = "x" in
  let new_value = Value.Bool true in
  Environment.empty ()
  |> (fun env -> Environment.define ~env ~id (Value.Number 42.))
  |> (fun env -> Environment.assign ~env ~id new_value)
  |> Option.bind ~f:(fun env -> Environment.get ~env ~id)
  |> check_maybe_value (Some new_value)

let declare_assign_with_different_scope () =
  let id = "x" in
  let new_value = Value.Bool true in
  let env =
    Environment.empty ()
    |> (fun env -> Environment.define ~env ~id (Value.Number 42.))
    |> (fun env -> Environment.push_scope ~env)
    |> fun env -> Environment.assign ~env ~id new_value
  in
  Option.bind env ~f:(fun env -> Environment.get ~env ~id)
  |> check_maybe_value (Some new_value) ;
  Option.bind env ~f:(fun env -> Environment.pop_scope ~env)
  |> Option.bind ~f:(fun (_, env) -> Environment.get ~env ~id)
  |> check_maybe_value (Some new_value)

let assign_without_declaration_fails () =
  Environment.assign ~env:(Environment.empty ()) ~id:"x" (Value.Number 42.)
  |> Option.is_none |> check_bool true

let all =
  [ Alcotest.test_case "Environment empty" `Quick environment_empty
  ; Alcotest.test_case "Environment with empty scopes" `Quick
      environment_with_empty_scopes
  ; Alcotest.test_case "Get unknown value is none" `Quick
      get_unknown_value_is_none
  ; Alcotest.test_case "Pop empty environment is none" `Quick
      pop_empty_environment_is_none
  ; Alcotest.test_case "Define and get value" `Quick define_and_get_value
  ; Alcotest.test_case "Define can shadow" `Quick define_can_shadow
  ; Alcotest.test_case "Define can shadow in new scope" `Quick
      define_can_shadow_in_new_scope
  ; Alcotest.test_case "Does contains" `Quick does_contains
  ; Alcotest.test_case "Does not contains" `Quick does_not_contains
  ; Alcotest.test_case "Declare assign with same scope" `Quick
      declare_assign_with_same_scope
  ; Alcotest.test_case "Assign without declaration fails" `Quick
      assign_without_declaration_fails
  ; Alcotest.test_case "Declare assign with different scope" `Quick
      declare_assign_with_different_scope ]
