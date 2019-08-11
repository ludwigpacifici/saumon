(** Represent an opaque value *)

type t =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
[@@deriving show, eq]

let to_string = function
  | Bool x -> string_of_bool x
  | Nil -> "Nil"
  | Number x -> string_of_float x
  | String x -> x

let of_ast_literal = function
  | Ast.Number x -> Number x
  | Ast.Identifier _ ->
      (* Should it use the name to resolve the value? *)
      failwith "Evaluating an identifier is not implemented."
  | Ast.String x -> String x
  | Ast.Bool x -> Bool x
  | Ast.Nil -> Nil
