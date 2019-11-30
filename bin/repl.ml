open Core

let ps1 elapsed errored counter exe_name =
  let l1 =
    "┏━━┫ ⮝ elapsed: "
    ^ elapsed
    ^ " ┣━━┫ ⮝ errored: "
    ^ Bool.to_string errored
    ^ " ┣━━┫ counter: "
    ^ Int.to_string counter
    ^ " ┃"
  in
  let l2 = "┗ " ^ exe_name ^ " ><> " in
  Out_channel.print_endline l1 ;
  Out_channel.print_string l2 ;
  Out_channel.flush stdout

let run exe_name args in_channel =
  let zero_time_span = Time_ns.Span.of_ns 0. in
  let rec loop elapsed errored counter =
    ps1 (Time_ns.Span.to_string elapsed) errored counter exe_name ;
    let elapsed, errored =
      match In_channel.input_line in_channel with
      | None -> Out_channel.newline stdout ; (zero_time_span, false)
      | Some code ->
          let start = Time_ns.now () in
          let code = Run.start args code in
          let errored = code <> Run.exit_code_to_enum Run.Success in
          (Time_ns.diff (Time_ns.now ()) start, errored)
    in
    loop elapsed errored (counter + 1)
  in
  loop zero_time_span false 0
