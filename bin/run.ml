open Core
open Saumon
open Saumon.Omnipresent

type args =
  { print_scanner : bool
  ; print_parser : bool }

let print_scanner tokens =
  Out_channel.print_endline "[SCANNER]" ;
  List.iter tokens ~f:(Token.show >> Out_channel.print_endline)

let print_parser ast =
  Out_channel.print_endline "[PARSER]" ;
  Ast.show_expression ast |> Out_channel.print_endline

let print_scanner_errors =
  List.iter ~f:(fun (x : Scanner.error) ->
      Display.error x.location ~where:x.where ~message:x.message )

let print_parser_errors (errs, t) =
  List.iter errs ~f:Out_channel.print_endline ;
  let t = Option.map t ~f:Token.show |> Option.value ~default:"<None>" in
  Out_channel.print_endline ("Token: " ^ t)

let print_interpreter_errors err =
  Out_channel.print_endline "[INTERPRETER]" ;
  Interpreter.show_error err |> Out_channel.print_endline

let start args data =
  Scanner.scan_tokens data
  |> Result.map_error ~f:print_scanner_errors
  |> Result.map ~f:(fun tokens ->
         if args.print_scanner then print_scanner tokens ;
         tokens )
  |> Result.bind ~f:(Parser.parse >> Result.map_error ~f:print_parser_errors)
  |> Result.map ~f:(fun ast ->
         if args.print_parser then print_parser ast ;
         ast )
  |> Result.bind
       ~f:(Interpreter.evaluate >> Result.map_error ~f:print_interpreter_errors)
  |> Result.map ~f:(Value.to_string >> Out_channel.print_endline)
  |> Result.is_error
