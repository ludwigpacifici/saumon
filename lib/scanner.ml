type t =
  { line : int
  ; column : int
  ; errored : bool }
[@@deriving show]

let make () = {line = 1; column = 0; errored = false}

open Base

let is_word_end = function
  | [] -> true
  | c :: _ when Char.is_alphanum c -> false
  | _ -> true

let add_token tokens kind line column =
  Token.make ~kind ~line ~column :: tokens

let rec loop scanner tokens = function
  | [] -> (add_token tokens EOF scanner.line scanner.column, scanner.errored)
  | '(' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens LEFT_PAREN scanner.line scanner.column)
        tl
  | ')' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens RIGHT_PAREN scanner.line scanner.column)
        tl
  | '{' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens LEFT_BRACE scanner.line scanner.column)
        tl
  | '}' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens RIGHT_BRACE scanner.line scanner.column)
        tl
  | ',' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens COMMA scanner.line scanner.column)
        tl
  | '.' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens DOT scanner.line scanner.column)
        tl
  | '-' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens MINUS scanner.line scanner.column)
        tl
  | '+' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens PLUS scanner.line scanner.column)
        tl
  | ';' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens SEMICOLON scanner.line scanner.column)
        tl
  | '/' :: '/' :: tl ->
      (* Discard data up to the end of the line or EOF *)
      let _, tl = List.split_while ~f:(fun c -> Char.compare '\n' c <> 0) tl in
      let scanner =
        if List.is_empty tl then scanner
        else {scanner with line = scanner.line + 1; column = 0}
      in
      loop scanner (add_token tokens BANG_EQUAL scanner.line scanner.column) tl
  | '/' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens SLASH scanner.line scanner.column)
        tl
  | '*' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens STAR scanner.line scanner.column)
        tl
  | '!' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens BANG_EQUAL scanner.line scanner.column)
        tl
  | '!' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens BANG scanner.line scanner.column)
        tl
  | '=' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens EQUAL_EQUAL scanner.line scanner.column)
        tl
  | '=' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens EQUAL scanner.line scanner.column)
        tl
  | '<' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens GREATER_EQUAL scanner.line scanner.column)
        tl
  | '<' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens GREATER scanner.line scanner.column)
        tl
  | '>' :: '=' :: tl ->
      loop
        {scanner with column = scanner.column + 2}
        (add_token tokens LESS_EQUAL scanner.line scanner.column)
        tl
  | '>' :: tl ->
      loop
        {scanner with column = scanner.column + 1}
        (add_token tokens LESS scanner.line scanner.column)
        tl
  | 'a' :: 'n' :: 'd' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length AND}
        (add_token tokens AND scanner.line scanner.column)
        tl
  | 'c' :: 'l' :: 'a' :: 's' :: 's' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length CLASS}
        (add_token tokens CLASS scanner.line scanner.column)
        tl
  | 'e' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length ELSE}
        (add_token tokens ELSE scanner.line scanner.column)
        tl
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length FALSE}
        (add_token tokens FALSE scanner.line scanner.column)
        tl
  | 'f' :: 'u' :: 'n' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length FUN}
        (add_token tokens FUN scanner.line scanner.column)
        tl
  | 'f' :: 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length FOR}
        (add_token tokens FOR scanner.line scanner.column)
        tl
  | 'i' :: 'f' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length IF}
        (add_token tokens IF scanner.line scanner.column)
        tl
  | 'n' :: 'i' :: 'l' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length NIL}
        (add_token tokens NIL scanner.line scanner.column)
        tl
  | 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length OR}
        (add_token tokens OR scanner.line scanner.column)
        tl
  | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length PRINT}
        (add_token tokens PRINT scanner.line scanner.column)
        tl
  | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length RETURN}
        (add_token tokens RETURN scanner.line scanner.column)
        tl
  | 's' :: 'u' :: 'p' :: 'e' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length SUPER}
        (add_token tokens SUPER scanner.line scanner.column)
        tl
  | 't' :: 'h' :: 'i' :: 's' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length THIS}
        (add_token tokens THIS scanner.line scanner.column)
        tl
  | 't' :: 'r' :: 'u' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length TRUE}
        (add_token tokens TRUE scanner.line scanner.column)
        tl
  | 'v' :: 'a' :: 'r' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length VAR}
        (add_token tokens VAR scanner.line scanner.column)
        tl
  | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: tl when is_word_end tl ->
      loop
        {scanner with column = scanner.column + Token.length WHILE}
        (add_token tokens WHILE scanner.line scanner.column)
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
        (add_token tokens (IDENTIFIER identifier) scanner.line scanner.column)
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
        (add_token tokens (STRING str) scanner.line scanner.column)
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
            (add_token tokens (NUMBER n) scanner.line scanner.column)
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
