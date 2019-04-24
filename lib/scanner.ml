open Base

type error =
  { location : Location.t
  ; where : string
  ; message : string }

let is_word_end = function
  | [] -> true
  | c :: _ when Char.is_alphanum c -> false
  | _ -> true

let make_add_token kind location tokens = Token.make ~kind ~location :: tokens

let rec loop location tokens = function
  | [] ->
      tokens
      |> Result.map ~f:(make_add_token Eof location)
      |> Result.map ~f:List.rev
      |> Result.map_error ~f:List.rev
  | '(' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Left_paren location))
        tl
  | ')' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Right_paren location))
        tl
  | '{' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Left_brace location))
        tl
  | '}' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Right_brace location))
        tl
  | ',' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Comma location))
        tl
  | '.' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Dot location))
        tl
  | '-' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Minus location))
        tl
  | '+' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Plus location))
        tl
  | ';' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Semicolon location))
        tl
  | '/' :: '/' :: tl -> (
    (* Discard data up to the end of the line or EOF *)
    match List.split_while ~f:(fun c -> Char.compare '\n' c <> 0) tl with
    | _, [] -> loop location tokens tl
    | _, _ -> loop (Location.next_line location) tokens tl )
  | '/' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Slash location))
        tl
  | '*' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Star location))
        tl
  | '!' :: '=' :: tl ->
      loop
        (Location.next_column location 2)
        (Result.map tokens ~f:(make_add_token Bang_equal location))
        tl
  | '!' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Bang location))
        tl
  | '=' :: '=' :: tl ->
      loop
        (Location.next_column location 2)
        (Result.map tokens ~f:(make_add_token Equal_equal location))
        tl
  | '=' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Equal location))
        tl
  | '<' :: '=' :: tl ->
      loop
        (Location.next_column location 2)
        (Result.map tokens ~f:(make_add_token Greater_equal location))
        tl
  | '<' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Greater location))
        tl
  | '>' :: '=' :: tl ->
      loop
        (Location.next_column location 2)
        (Result.map tokens ~f:(make_add_token Less_equal location))
        tl
  | '>' :: tl ->
      loop
        (Location.next_column location 1)
        (Result.map tokens ~f:(make_add_token Less location))
        tl
  | 'a' :: 'n' :: 'd' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 3)
        (Result.map tokens ~f:(make_add_token And location))
        tl
  | 'c' :: 'l' :: 'a' :: 's' :: 's' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 5)
        (Result.map tokens ~f:(make_add_token Class location))
        tl
  | 'e' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 4)
        (Result.map tokens ~f:(make_add_token Else location))
        tl
  | 'f' :: 'a' :: 'l' :: 's' :: 'e' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 5)
        (Result.map tokens ~f:(make_add_token False location))
        tl
  | 'f' :: 'u' :: 'n' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 3)
        (Result.map tokens ~f:(make_add_token Fun location))
        tl
  | 'f' :: 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 3)
        (Result.map tokens ~f:(make_add_token For location))
        tl
  | 'i' :: 'f' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 2)
        (Result.map tokens ~f:(make_add_token If location))
        tl
  | 'n' :: 'i' :: 'l' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 3)
        (Result.map tokens ~f:(make_add_token Nil location))
        tl
  | 'o' :: 'r' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 2)
        (Result.map tokens ~f:(make_add_token Or location))
        tl
  | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 5)
        (Result.map tokens ~f:(make_add_token Print location))
        tl
  | 'r' :: 'e' :: 't' :: 'u' :: 'r' :: 'n' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 6)
        (Result.map tokens ~f:(make_add_token Return location))
        tl
  | 's' :: 'u' :: 'p' :: 'e' :: 'r' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 5)
        (Result.map tokens ~f:(make_add_token Super location))
        tl
  | 't' :: 'h' :: 'i' :: 's' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 4)
        (Result.map tokens ~f:(make_add_token This location))
        tl
  | 't' :: 'r' :: 'u' :: 'e' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 4)
        (Result.map tokens ~f:(make_add_token True location))
        tl
  | 'v' :: 'a' :: 'r' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 3)
        (Result.map tokens ~f:(make_add_token Var location))
        tl
  | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: tl when is_word_end tl ->
      loop
        (Location.next_column location 5)
        (Result.map tokens ~f:(make_add_token While location))
        tl
  | '\n' :: tl -> loop (Location.next_line location) tokens tl
  | c :: tl when Char.is_whitespace c ->
      loop (Location.next_column location 1) tokens tl
  | c :: _ as tl when Char.is_alpha c ->
      let identifier, tl = List.split_while ~f:Char.is_alphanum tl in
      let identifier = String.of_char_list identifier in
      loop
        (Location.next_column location (String.length identifier))
        (Result.map tokens ~f:(make_add_token (Identifier identifier) location))
        tl
  | c :: tl when Char.equal '"' c ->
      (* Note the first '"' is discarded *)
      let str, tl =
        List.split_while ~f:(fun c -> Char.compare '"' c <> 0) tl
      in
      let str = String.of_char_list str in
      loop
        (* Take into consideration the two '"' *)
        (Location.next_column location (String.length str + 2))
        (Result.map tokens ~f:(make_add_token (String str) location))
        (* Discard the 2nd '"' *)
        (match tl with [] -> [] | _ :: tl -> tl)
  | c :: _ as tl when Char.is_digit c ->
      let raw_number, tl =
        List.split_while ~f:(fun c -> Char.is_digit c || Char.equal '.' c) tl
      in
      let raw_number = String.of_char_list raw_number in
      let len = String.length raw_number in
      let token =
        ( try Ok (Float.of_string raw_number)
          with _ ->
            Error
              [ { location
                ; where = raw_number
                ; message = "Expected to parse a float." } ] )
        |> Result.map ~f:(fun n ->
               Token.make ~kind:(Token_kind.Number n) ~location )
      in
      loop
        (Location.next_column location len)
        (Result.combine token tokens ~ok:List.cons ~err:List.append)
        tl
  | c :: tl ->
      let err =
        Error [{location; where = String.of_char c; message = "Unknown input"}]
      in
      loop
        (Location.next_column location 1)
        (Result.combine err tokens ~ok:List.cons ~err:List.append)
        tl

let scan_tokens code = loop (Location.start ()) (Ok []) (String.to_list code)
