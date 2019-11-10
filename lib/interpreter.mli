type error =
  { location : Location.t
  ; message : string }
[@@deriving show, eq]

open Base

(* Evaluate an expression and return its reduced form as `Value.t` and the
   updated Environment *)
val evaluate :
  Environment.t -> Ast.expression -> (Environment.t * Value.t, error) Result.t

(* Execute a program with a given environment. The continuation enables custom
   behavior (for example, usefull for testing) to process the program steps. *)
val execute_k :
     k:(Environment.t -> Value.t list -> unit)
  -> Environment.t
  -> Ast.Program.t
  -> (unit, error) Result.t

(* Execute a program with a given environment. Returns a unit (since it relies
   on side effects) or the first error. Note: it is equivalent to executek with
   a continuation printing everything to the standard output. *)
val execute : Environment.t -> Ast.Program.t -> (unit, error) Result.t
