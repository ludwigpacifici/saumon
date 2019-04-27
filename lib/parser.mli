open Base

val parse :
  Token.t list -> (Ast.expression, string list * Token.t option) Result.t
