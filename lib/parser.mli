open Base

val parse :
  Token.t list -> (Ast.program, string list * Token.t option) Result.t
