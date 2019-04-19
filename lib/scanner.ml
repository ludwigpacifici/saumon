type t =
  { line : int
  ; column : int
  ; errored : bool }
[@@deriving show]

let make () = {line = 1; column = 0; errored = false}

open Base
open Omnipresent

let is_word_end = function
  | [] -> true
  | c :: _ when Char.is_alphanum c -> false
  | _ -> true

let add_token tokens kind line column =
  Token.make ~kind ~line ~column :: tokens

let token_length = Token_kind.to_string >> String.length

let rec loop scanner tokens = function
  | [] -> (add_token tokens Eof scanner.line scanner.column, scanner.errored)
  | '(' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Left_paren scanner.line scanner.column)
        tl
  | ')' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Right_paren scanner.line scanner.column)
        tl
  | '{' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Left_brace scanner.line scanner.column)
        tl
  | '}' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Right_brace scanner.line scanner.column)
        tl
  | ',' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Comma scanner.line scanner.column)
        tl
  | '.' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Dot scanner.line scanner.column)
        tl
  | '-' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Minus scanner.line scanner.column)
        tl
  | '+' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Plus scanner.line scanner.column)
        tl
  | ';' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Semicolon scanner.line scanner.column)
        tl
  | '/' :: '/' :: tl -> (
    (* Discard data up to the end of the line or EOF *)
    match List.split_while ~f:(fun c -> Char.compare '\n' c <> 0) tl with
    | _, [] -> loop scanner tokens tl
    | _, _ -> loop {scanner with line = scanner.line + 1; column = 0} tokens tl
    )
  | '/' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Slash scanner.line scanner.column)
        tl
  | '*' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Star scanner.line scanner.column)
        tl
  | '!' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens Bang_equal scanner.line scanner.column)
        tl
  | '!' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Bang scanner.line scanner.column)
        tl
  | '=' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens Equal_equal scanner.line scanner.column)
        tl
  | '=' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Equal scanner.line scanner.column)
        tl
  | '<' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens Greater_equal scanner.line scanner.column)
        tl
  | '<' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Greater scanner.line scanner.column)
        tl
  | '>' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens Less_equal scanner.line scanner.column)
        tl
  | '>' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens Less scanner.line scanner.column)
        tl
  | 'a' :: 'n' :: 'd' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length And}
        (add_token tokens And scanner.line scanner.column)
        tl
  | 'c' :: 'l' :: 'a' :: 's' :: 's' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Class}
        (add_token tokens Class scanner.line scanner.column)
        tl
  | 'e' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Else}
        (add_token tokens Else scanner.line scanner.column)
        tl
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length False}
        (add_token tokens False scanner.line scanner.column)
        tl
  | 'f' :: 'u' :: 'n' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Fun}
        (add_token tokens Fun scanner.line scanner.column)
        tl
  | 'f' :: 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length For}
        (add_token tokens For scanner.line scanner.column)
        tl
  | 'i' :: 'f' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length If}
        (add_token tokens If scanner.line scanner.column)
        tl
  | 'n' :: 'i' :: 'l' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Nil}
        (add_token tokens Nil scanner.line scanner.column)
        tl
  | 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Or}
        (add_token tokens Or scanner.line scanner.column)
        tl
  | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Print}
        (add_token tokens Print scanner.line scanner.column)
        tl
  | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Return}
        (add_token tokens Return scanner.line scanner.column)
        tl
  | 's' :: 'u' :: 'p' :: 'e' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Super}
        (add_token tokens Super scanner.line scanner.column)
        tl
  | 't' :: 'h' :: 'i' :: 's' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length This}
        (add_token tokens This scanner.line scanner.column)
        tl
  | 't' :: 'r' :: 'u' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length True}
        (add_token tokens True scanner.line scanner.column)
        tl
  | 'v' :: 'a' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length Var}
        (add_token tokens Var scanner.line scanner.column)
        tl
  | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + token_length While}
        (add_token tokens While scanner.line scanner.column)
        tl
  | '\n' :: tl ->
      loop {scanner with line = scanner.line + 1; column = 0} tokens tl
  | c :: tl when Char.is_whitespace c ->
      loop {scanner with column = scanner.column + 1} tokens tl
  | c :: _ as tl when Char.is_alpha c ->
      let identifier, tl = List.split_while ~f:Char.is_alphanum tl in
      let identifier = String.of_char_list identifier in
      loop
        {scanner with column = scanner.column + String.length identifier}
        (add_token tokens (Identifier identifier) scanner.line scanner.column)
        tl
  | c :: tl when Char.equal '"' c ->
      (* Note the first '"' is discarded *)
      let str, tl =
        List.split_while ~f:(fun c -> Char.compare '"' c <> 0) tl
      in
      let str = String.of_char_list str in
      loop
        (* Take into consideration the two '"' *)
        {scanner with column = scanner.column + String.length str + 2}
        (add_token tokens (String str) scanner.line scanner.column)
        (* Discard the 2nd '"' *)
        (match tl with [] -> [] | _ :: tl -> tl)
  | c :: _ as tl when Char.is_digit c -> (
      let number, tl =
        List.split_while ~f:(fun c -> Char.is_digit c || Char.equal '.' c) tl
      in
      let len = List.length number in
      let number =
        try number |> String.of_char_list |> Float.of_string |> Option.some
        with _ -> None
      in
      match number with
      | Some n ->
          loop
            {scanner with column = scanner.column + len}
            (add_token tokens (Number n) scanner.line scanner.column)
            tl
      | None ->
          loop
            {scanner with column = scanner.column + len; errored = true}
            tokens tl )
  | c :: tl ->
      Display.error ~line:scanner.line ~where:(String.of_char c)
        ~message:"Unknown input" ;
      loop {scanner with column = scanner.column + 1; errored = true} tokens tl

let scan_tokens code =
  let scanner = make () in
  let tokens, errored = loop scanner [] (String.to_list code) in
  (List.rev tokens, errored)
