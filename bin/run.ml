open Core
open Saumon
open Saumon.Omnipresent

type args =
  { print_scanner : bool
  ; print_parser : bool }

type exit_code =
  | Success
  | ScannerError
  | ParserError
  | InterpreterError
[@@deriving enum]

let print_scanner tokens =
  Out_channel.print_endline "[SCANNER]" ;
  List.iter tokens ~f:(Token.show >> Out_channel.print_endline)

let print_parser ast =
  Out_channel.print_endline "[PARSER]" ;
  Ast.Program.show ast |> Out_channel.print_endline

let print_scanner_errors =
  List.iter ~f:(fun (x : Scanner.error) ->
      Display.error x.location ~where:x.where ~message:x.message)

let print_parser_errors (errs, t) =
  List.iter errs ~f:Out_channel.print_endline ;
  let t = Option.map t ~f:Token.show |> Option.value ~default:"<None>" in
  Out_channel.print_endline ("Token: " ^ t)

let print_interpreter_errors (err : Interpreter.error) =
  Out_channel.print_endline "[INTERPRETER]" ;
  Display.error err.location ~where:err.where ~message:err.message

let start args data =
  let exit_code =
    let tokens =
      Scanner.scan_tokens data
      |> Result.map_error ~f:(fun err ->
             print_scanner_errors err ; ScannerError)
    in
    Result.iter tokens ~f:(fun tokens ->
        if args.print_scanner then print_scanner tokens) ;
    let ast =
      Result.bind tokens
        ~f:
          ( Parser.parse
          >> Result.map_error ~f:(fun err ->
                 print_parser_errors err ; ParserError) )
    in
    Result.iter ast ~f:(fun ast -> if args.print_parser then print_parser ast) ;
    Result.bind ast
      ~f:
        ((* TODO: For now there is only one global environment *)
         let env = Environment.empty () in
         Interpreter.execute env
         >> Result.map_error ~f:(fun err ->
                print_interpreter_errors err ;
                InterpreterError))
    |> Result.map ~f:(fun () -> Success)
  in
  match exit_code with
  | Ok code -> exit_code_to_enum code
  | Error code -> exit_code_to_enum code
