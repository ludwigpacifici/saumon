(** Represent an opaque value *)

type t =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
[@@deriving show, eq]

let to_string = function
  | Number x -> string_of_float x
  | String x -> x
  | Bool x -> string_of_bool x
  | Nil -> "Nil"

let of_ast_literal = function
  | Ast.Number x -> Number x
  | Ast.String x -> String x
  | Ast.Bool x -> Bool x
  | Ast.Nil -> Nil
