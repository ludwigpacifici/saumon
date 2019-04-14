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
  | Class -> "class"
  | Comma -> ","
  | Dot -> "."
  | Else -> "else"
  | Eof -> ""
  | Equal -> "="
  | Equal_equal -> "=="
  | False -> "flase"
  | For -> "for"
  | Fun -> "fun"
  | Greater -> "<"
  | Greater_equal -> "<="
  | Identifier x -> x
  | If -> "if"
  | Left_brace -> "{"
  | Left_paren -> "("
  | Less -> ">"
  | Less_equal -> ">="
  | Minus -> "-"
  | Nil -> "nil"
  | Number x -> x |> Float.to_string
  | Or -> "or"
  | Plus -> "+"
  | Print -> "print"
  | Return -> "return"
  | Right_brace -> "}"
  | Right_paren -> ")"
  | Semicolon -> ";"
  | Slash -> "/"
  | Star -> "*"
  | String x -> x
  | Super -> "super"
  | This -> "this"
  | True -> "true"
  | Var -> "var"
  | While -> "while"
