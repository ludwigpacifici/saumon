type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show]

open Base

let rec evaluate = function
  | Ast.Literal x -> Ok (Value.of_ast_literal x)
  | Ast.Grouping (_, x, _) -> evaluate x
  | Ast.Unary (t, x) -> evaluate_unary t x
  | Ast.Binary (l, t, r) -> evaluate_binary l t r

and evaluate_unary t x =
  match (t.kind, evaluate x) with
  | Token_kind.Minus, Ok (Value.Number x) -> Ok (Value.Number (-.x))
  (* Lox follows Ruby’s simple rule: false and nil are falsey and everything
     else is truthy. *)
  | Token_kind.Bang, Ok (Value.Bool false) -> Ok (Value.Bool true)
  | Token_kind.Bang, Ok Value.Nil -> Ok (Value.Bool true)
  | Token_kind.Bang, _ -> Ok (Value.Bool false)
  | _ -> Error {location = t.location; where = "fix me"; message = "fix me"}

and evaluate_binary l t r =
  match (evaluate l, t.kind, evaluate r) with
  | Ok l, Token_kind.Equal_equal, Ok r -> Ok (Value.Bool (Value.equal l r))
  | Ok l, Token_kind.Bang_equal, Ok r ->
      let open Omnipresent in
      Ok (Value.Bool (not <| Value.equal l r))
  | Ok (Value.Number l), t_kind, Ok (Value.Number r) -> (
      let open Poly in
      match t_kind with
      | Token_kind.Plus -> Ok (Value.Number (l +. r))
      | Token_kind.Minus -> Ok (Value.Number (l -. r))
      | Token_kind.Star -> Ok (Value.Number (l *. r))
      | Token_kind.Slash -> Ok (Value.Number (l /. r))
      | Token_kind.Greater -> Ok (Value.Bool (l > r))
      | Token_kind.Greater_equal -> Ok (Value.Bool (l >= r))
      | Token_kind.Less -> Ok (Value.Bool (l < r))
      | Token_kind.Less_equal -> Ok (Value.Bool (l <= r))
      | _ -> Error {location = t.location; where = "fix me"; message = "fix me"}
      )
  | Ok (Value.String l), t_kind, Ok (Value.String r) -> (
    match t_kind with
    | Token_kind.Plus -> Ok (Value.String (l ^ r))
    | _ -> Error {location = t.location; where = "fix me"; message = "fix me"}
    )
  | _ -> Error {location = t.location; where = "fix me"; message = "fix me"}
