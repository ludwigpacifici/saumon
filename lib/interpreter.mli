type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show]

open Base

val evaluate : Ast.expression -> (Value.t, error) Result.t
