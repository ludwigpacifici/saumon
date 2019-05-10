type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show]

open Base

let evaluate_binary_number l (t : Token.t) r =
  let open Poly in
  match t.kind with
  | Token_kind.Plus -> Ok (Value.Number (l +. r))
  | Token_kind.Minus -> Ok (Value.Number (l -. r))
  | Token_kind.Star -> Ok (Value.Number (l *. r))
  | Token_kind.Slash -> Ok (Value.Number (l /. r))
  | Token_kind.Greater -> Ok (Value.Bool (l > r))
  | Token_kind.Greater_equal -> Ok (Value.Bool (l >= r))
  | Token_kind.Less -> Ok (Value.Bool (l < r))
  | Token_kind.Less_equal -> Ok (Value.Bool (l <= r))
  | t_kind ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Unknown binary operator to be used with two floats" }

let evaluate_binary_string l (t : Token.t) r =
  match t.kind with
  | Token_kind.Plus -> Ok (Value.String (l ^ r))
  | t_kind ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Unknown binary operator to be used with two strings" }

let rec evaluate = function
  | Ast.Literal x -> evaluate_literal x
  | Ast.Grouping (_, x, _) -> evaluate x
  | Ast.Unary (t, x) -> evaluate_unary t x
  | Ast.Binary (l, t, r) -> evaluate_binary l t r

and evaluate_literal x = Ok (Value.of_ast_literal x)

and evaluate_unary t x =
  match (t.kind, evaluate x) with
  | Token_kind.Minus, Ok (Value.Number x) -> Ok (Value.Number (-.x))
  | Token_kind.Minus, Ok _ ->
      Error
        { location = t.location
        ; where = "-"
        ; message = "Expected a float after a unary minus" }
  (* Lox follows Ruby’s simple rule: false and nil are falsey and everything
     else is truthy. *)
  | Token_kind.Bang, Ok (Value.Bool false) -> Ok (Value.Bool true)
  | Token_kind.Bang, Ok Value.Nil -> Ok (Value.Bool true)
  | Token_kind.Bang, _ -> Ok (Value.Bool false)
  | t_kind, _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Unknown unary operator" }

and evaluate_binary l t r =
  match (evaluate l, t.kind, evaluate r) with
  | Ok l, Token_kind.Equal_equal, Ok r -> Ok (Value.Bool (Value.equal l r))
  | Ok l, Token_kind.Bang_equal, Ok r ->
      Ok (Value.Bool (Value.equal l r |> not))
  | Ok (Value.Number l), _, Ok (Value.Number r) -> evaluate_binary_number l t r
  | Ok (Value.String l), _, Ok (Value.String r) -> evaluate_binary_string l t r
  | Ok _, t_kind, Ok _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message =
            "Unknown binary operator when left and right expressions have \
             different types" }
  | Error _, t_kind, Ok _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Not able to evaluate the left expression" }
  | Ok _, t_kind, Error _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Not able to evaluate the right expression" }
  | Error _, t_kind, Error _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Not able to evaluate both left and right expression" }
