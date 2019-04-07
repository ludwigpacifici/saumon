open Core

let error ~line ~where ~message =
  "error: " ^ message |> Out_channel.print_endline ;
  "-> line:" ^ Int.to_string line ^ ", where: " ^ where
  |> Out_channel.print_endline
