type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show]

open Base

(* Evaluate an expression and return its reduced form as `Value.t` *)
val evaluate : Ast.expression -> (Value.t, error) Result.t

(* Execute a program, i.e. a statement list. Returns a unit (since it relies on
   side effects) or the first error. *)
val execute : Ast.program -> (unit, error) Result.t
