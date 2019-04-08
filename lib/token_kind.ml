type t =
  | Left_paren
  | Right_paren
  | Left_brace
  | Right_brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | Bang_equal
  | Equal
  | Equal_equal
  | Greater
  | Greater_equal
  | Less
  | Less_equal
  | Identifier of string
  | String of string
  | Number of float
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Eof
[@@deriving show]

let length = function
  | Eof -> 0
  | Bang | Comma | Dot | Equal | Greater | Left_brace | Left_paren | Less -> 1
  | Minus | Plus | Right_brace | Right_paren | Semicolon | Slash | Star -> 1
  | If | Or | Bang_equal | Equal_equal | Greater_equal | Less_equal -> 2
  | And | For | Fun | Nil | Var -> 3
  | Else | This | True -> 4
  | Class | False | Print | Super | While -> 5
  | Return -> 6
  | Identifier x | String x -> String.length x
  | Number x -> x |> Float.to_string |> String.length
