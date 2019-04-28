open Core
open Saumon.Omnipresent

type args =
  { print_scanner : bool
  ; print_parser : bool }

let print_scanner tokens =
  let open Saumon in
  Out_channel.print_endline "[SCANNER]" ;
  List.iter tokens ~f:(Token.show >> Out_channel.print_endline)

let print_parser ast =
  let open Saumon in
  Out_channel.print_endline "[PARSER]" ;
  match ast with
  | Ok e -> Ast.show_expression e |> Out_channel.print_endline
  | Error (errs, t) ->
      List.iter errs ~f:Out_channel.print_endline ;
      let t = Option.map t ~f:Token.show |> Option.value ~default:"<None>" in
      Out_channel.print_endline ("Token: " ^ t)

let start args data =
  let open Saumon in
  match Scanner.scan_tokens data with
  | Ok tokens ->
      if args.print_scanner then print_scanner tokens ;
      if args.print_parser then Parser.parse tokens |> print_parser ;
      false
  | Error errors ->
      List.iter errors ~f:(fun x ->
          Display.error x.location ~where:x.where ~message:x.message ) ;
      true
