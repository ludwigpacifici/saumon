open Base
open Saumon
module A = Alcotest_testable

let check_value_list =
  Alcotest.(check (list A.value)) "check Interpreter.execute_k"

let check_option_value =
  Alcotest.(check (option A.value)) "check Interpreter.execute_k"

let check_true = Alcotest.(check bool) "check Interpreter.execute_k" true

(* Generate the ast from a string of Lox code. Useful for brief and comprehensives tests. Raise `invalid_arg` either
   when the scanner or the parser fails. *)
let make_program_exn (str : string) : Ast.Program.t =
  Scanner.scan_tokens str |> Caml.Result.get_ok |> Parser.parse
  |> Caml.Result.get_ok

(* Execute a program always with an empty environment. Useful for brief and comprehensives tests. *)
let execute_with_empty_env ~(k : Environment.t -> Value.t list -> unit)
    (p : Ast.Program.t) =
  let env = Environment.empty () in
  Interpreter.execute_k ~k env p

(* Ignore for function with two parameters. *)
let ignore2 _ = ignore
