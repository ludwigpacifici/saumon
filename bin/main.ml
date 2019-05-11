open Core
open Saumon.Omnipresent

type input_mode =
  | Script
  | Repl

let main_exn exe_name args in_channel input_mode : int =
  match input_mode with
  | Repl -> Repl.run exe_name args in_channel
  | Script -> Script.run args in_channel

let resolve_in_channel = function
  | "-" -> (In_channel.stdin, Repl)
  | filename -> (In_channel.create ~binary:false filename, Script)

let command exe_name =
  Command.basic
    ~summary:(exe_name ^ ": An interpreter for Lox")
    Command.Let_syntax.(
      let%map_open filename =
        anon (maybe_with_default "-" ("filename" %: string))
      and print_scanner =
        flag "-s" no_arg ~doc:" Print scanner outputs on stdout"
      and print_parser =
        flag "-p" no_arg ~doc:" Print parser outputs on stdout"
      in
      fun () ->
        let args : Run.args = {print_scanner; print_parser} in
        resolve_in_channel filename ||> main_exn exe_name args |> exit)

let () =
  let exe_name = Filename.basename Sys.argv.(0) in
  Command.run (command exe_name)
