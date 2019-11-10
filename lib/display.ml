open Core

let error location ~message =
  "error: " ^ message |> Out_channel.print_endline ;
  "-> " ^ Location.show_compact location |> Out_channel.print_endline
