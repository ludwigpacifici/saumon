type t =
  | And
  | Bang
  | Bang_equal
  | Class
  | Comma
  | Dot
  | Else
  | Eof
  | Equal
  | Equal_equal
  | False
  | For
  | Fun
  | Greater
  | Greater_equal
  | Identifier of string
  | If
  | Left_brace
  | Left_paren
  | Less
  | Less_equal
  | Minus
  | Nil
  | Number of float
  | Or
  | Plus
  | Print
  | Return
  | Right_brace
  | Right_paren
  | Semicolon
  | Slash
  | Star
  | String of string
  | Super
  | This
  | True
  | Var
  | While
[@@deriving show, eq]
