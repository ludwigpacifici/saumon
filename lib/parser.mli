open Base

val parse :
  Token.t list -> (Ast.Program.t, string list * Token.t option) Result.t
