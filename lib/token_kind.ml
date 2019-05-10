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

let to_string = function
  | And -> "and"
  | Bang -> "!"
  | Bang_equal -> "!="
  | Class -> "classe"
  | Comma -> ","
  | Dot -> "."
  | Else -> "else"
  | Eof -> "<eof>"
  | Equal -> "="
  | Equal_equal -> "=="
  | False -> "false"
  | For -> "for"
  | Fun -> "fun"
  | Greater -> ">"
  | Greater_equal -> ">="
  | Identifier s -> s
  | If -> "if"
  | Left_brace -> "{"
  | Left_paren -> "("
  | Less -> "<"
  | Less_equal -> "<="
  | Minus -> "-"
  | Nil -> "nil"
  | Number x -> Float.to_string x
  | Or -> "or"
  | Plus -> "+"
  | Print -> "print"
  | Return -> "return"
  | Right_brace -> "}"
  | Right_paren -> ")"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  | String s -> s
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
