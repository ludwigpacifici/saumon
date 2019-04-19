type t =
  { location : Location.t
  ; errored : bool }
[@@deriving show]

let make () = {location = Location.start (); errored = false}

open Base

let is_word_end = function
  | [] -> true
  | c :: _ when Char.is_alphanum c -> false
  | _ -> true

let add_token tokens kind location = Token.make ~kind ~location :: tokens

let rec loop scanner tokens = function
  | [] -> (add_token tokens Eof scanner.location, scanner.errored)
  | '(' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Left_paren scanner.location)
        tl
  | ')' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Right_paren scanner.location)
        tl
  | '{' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Left_brace scanner.location)
        tl
  | '}' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Right_brace scanner.location)
        tl
  | ',' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Comma scanner.location)
        tl
  | '.' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Dot scanner.location)
        tl
  | '-' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Minus scanner.location)
        tl
  | '+' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Plus scanner.location)
        tl
  | ';' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Semicolon scanner.location)
        tl
  | '/' :: '/' :: tl -> (
    (* Discard data up to the end of the line or EOF *)
    match List.split_while ~f:(fun c -> Char.compare '\n' c <> 0) tl with
    | _, [] -> loop scanner tokens tl
    | _, _ ->
        loop
          {scanner with location = Location.next_line scanner.location}
          tokens tl )
  | '/' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Slash scanner.location)
        tl
  | '*' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Star scanner.location)
        tl
  | '!' :: '=' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 2}
        (add_token tokens Bang_equal scanner.location)
        tl
  | '!' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Bang scanner.location)
        tl
  | '=' :: '=' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 2}
        (add_token tokens Equal_equal scanner.location)
        tl
  | '=' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Equal scanner.location)
        tl
  | '<' :: '=' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 2}
        (add_token tokens Greater_equal scanner.location)
        tl
  | '<' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Greater scanner.location)
        tl
  | '>' :: '=' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 2}
        (add_token tokens Less_equal scanner.location)
        tl
  | '>' :: tl ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        (add_token tokens Less scanner.location)
        tl
  | 'a' :: 'n' :: 'd' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 3}
        (add_token tokens And scanner.location)
        tl
  | 'c' :: 'l' :: 'a' :: 's' :: 's' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 5}
        (add_token tokens Class scanner.location)
        tl
  | 'e' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 4}
        (add_token tokens Else scanner.location)
        tl
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 5}
        (add_token tokens False scanner.location)
        tl
  | 'f' :: 'u' :: 'n' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 3}
        (add_token tokens Fun scanner.location)
        tl
  | 'f' :: 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 3}
        (add_token tokens For scanner.location)
        tl
  | 'i' :: 'f' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 2}
        (add_token tokens If scanner.location)
        tl
  | 'n' :: 'i' :: 'l' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 3}
        (add_token tokens Nil scanner.location)
        tl
  | 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 2}
        (add_token tokens Or scanner.location)
        tl
  | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 5}
        (add_token tokens Print scanner.location)
        tl
  | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 6}
        (add_token tokens Return scanner.location)
        tl
  | 's' :: 'u' :: 'p' :: 'e' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 5}
        (add_token tokens Super scanner.location)
        tl
  | 't' :: 'h' :: 'i' :: 's' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 4}
        (add_token tokens This scanner.location)
        tl
  | 't' :: 'r' :: 'u' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 4}
        (add_token tokens True scanner.location)
        tl
  | 'v' :: 'a' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 3}
        (add_token tokens Var scanner.location)
        tl
  | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with location = Location.next_column scanner.location 5}
        (add_token tokens While scanner.location)
        tl
  | '\n' :: tl ->
      loop
        {scanner with location = Location.next_line scanner.location}
        tokens tl
  | c :: tl when Char.is_whitespace c ->
      loop
        {scanner with location = Location.next_column scanner.location 1}
        tokens tl
  | c :: _ as tl when Char.is_alpha c ->
      let identifier, tl = List.split_while ~f:Char.is_alphanum tl in
      let identifier = String.of_char_list identifier in
      loop
        { scanner with
          location =
            Location.next_column scanner.location (String.length identifier) }
        (add_token tokens (Identifier identifier) scanner.location)
        tl
  | c :: tl when Char.equal '"' c ->
      (* Note the first '"' is discarded *)
      let str, tl =
        List.split_while ~f:(fun c -> Char.compare '"' c <> 0) tl
      in
      let str = String.of_char_list str in
      loop
        (* Take into consideration the two '"' *)
        { scanner with
          location =
            Location.next_column scanner.location (String.length str + 2) }
        (add_token tokens (String str) scanner.location)
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
            {scanner with location = Location.next_column scanner.location len}
            (add_token tokens (Number n) scanner.location)
            tl
      | None ->
          loop
            { location = Location.next_column scanner.location len
            ; errored = true }
            tokens tl )
  | c :: tl ->
      Display.error scanner.location ~where:(String.of_char c)
        ~message:"Unknown input" ;
      loop
        {location = Location.next_column scanner.location 1; errored = true}
        tokens tl

let scan_tokens code =
  let scanner = make () in
  let tokens, errored = loop scanner [] (String.to_list code) in
  (List.rev tokens, errored)
