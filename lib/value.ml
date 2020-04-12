(** Represent an opaque value *)

type t =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
[@@deriving show, eq]

open Base

let to_string = function
  | Bool x -> Bool.to_string x
  | Nil -> "nil"
  | Number x -> Float.to_string x
  | String x -> x

(** Lox follows Ruby’s simple rule: false and nil are falsey and everything
    else is truthy. *)
let is_truthy = function
  | Nil | Bool false -> false
  | Bool true | Number _ | String _ -> true

let of_ast_literal = function
  | Ast.Number x -> Number x
  | Ast.Identifier _ ->
      failwith
        "Evaluating an identifier will lead to dependency cycle between with \
         Environment."
  | Ast.String x -> String x
  | Ast.Bool x -> Bool x
  | Ast.Nil -> Nil

let of_bool x = Bool x
