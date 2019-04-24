open Core
open Saumon.Omnipresent

let usage exe_name = "Usage: " ^ exe_name ^ " [file]"

let run data =
  let open Saumon in
  match Scanner.scan_tokens data with
  | Ok tokens ->
      Out_channel.print_endline "[SCANNER]" ;
      List.iter tokens ~f:(Token.show >> Out_channel.print_endline) ;
      Out_channel.print_endline "[PARSER]" ;
      let expression = Parser.parse tokens in
      Out_channel.print_endline (Ast.show_expression expression) ;
      false
  | Error errors ->
      List.iter errors ~f:(fun x ->
          Display.error x.location ~where:x.where ~message:x.message ) ;
      true

let run_file ~file = file |> In_channel.read_all |> run

let run_repl exe_name =
  let zero_time_span = Time_ns.Span.of_ns 0. in
  let ps1 elapsed errored counter exe_name =
    let l1 =
      "┏━━┫ ⮝ elapsed: " ^ elapsed ^ " ┣━━┫ ⮝ errored: "
      ^ Bool.to_string errored ^ " ┣━━┫ counter: "
      ^ Int.to_string counter ^ " ┃"
    in
    let l2 = "┗ " ^ exe_name ^ " ><> " in
    Out_channel.print_endline l1 ;
    Out_channel.print_string l2 ;
    Out_channel.flush stdout
  in
  let rec loop elapsed errored counter =
    ps1 (Time_ns.Span.to_string elapsed) errored counter exe_name ;
    let elapsed, errored =
      match In_channel.(input_line stdin) with
      | None -> Out_channel.newline stdout ; (zero_time_span, false)
      | Some code ->
          let start = Time_ns.now () in
          let errored = run code in
          (Time_ns.diff (Time_ns.now ()) start, errored)
    in
    loop elapsed errored (counter + 1)
  in
  loop zero_time_span false 0

type exit_code =
  | Success
  | WrongArguments
  | LexerError
[@@deriving enum]

let main_exn () =
  let exe_name = Filename.basename Sys.argv.(0) in
  match Array.length Sys.argv with
  | 1 -> if run_repl exe_name then LexerError else Success
  | 2 -> if run_file ~file:Sys.argv.(1) then LexerError else Success
  | _ ->
      usage exe_name |> Out_channel.print_endline ;
      WrongArguments

let () =
  match main_exn () with
  | Success as exit_code -> exit_code |> exit_code_to_enum |> exit
  | exit_code ->
      let exit_code = exit_code |> exit_code_to_enum |> Int.to_string in
      "Exited with code: " ^ exit_code |> print_endline
