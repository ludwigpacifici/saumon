open Base

val parse : Token.t list -> (Ast.Program.t, string) Result.t
