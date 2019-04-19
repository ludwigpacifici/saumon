open Core

let error location ~where ~message =
  "error: " ^ message |> Out_channel.print_endline ;
  "-> " ^ Location.show_compact location ^ " where: " ^ where
  |> Out_channel.print_endline
