let rec evaluate = function
  | Ast.Literal x -> Value.of_ast_literal x
  | Ast.Grouping (_, x, _) -> evaluate x
  | Ast.Unary (t, x) -> evaluate_unary t x
  | Ast.Binary (l, t, r) -> evaluate_binary l t r

and evaluate_unary t x =
  match (t.kind, evaluate x) with
  | Token_kind.Minus, Value.Number x -> Value.Number (-.x)
  (* Lox follows Ruby’s simple rule: false and nil are falsey and everything
     else is truthy. *)
  | Token_kind.Bang, Value.Bool false -> Value.Bool true
  | Token_kind.Bang, Value.Nil -> Value.Bool true
  | Token_kind.Bang, _ -> Value.Bool false
  | _ -> failwith "not implemented"

and evaluate_binary l t r =
  match (evaluate l, t.kind, evaluate r) with
  | l, Token_kind.Equal_equal, r -> Value.Bool (Value.equal l r)
  | l, Token_kind.Bang_equal, r ->
      let open Omnipresent in
      Value.Bool (not <| Value.equal l r)
  | Value.Number l, t, Value.Number r -> (
    match t with
    | Token_kind.Plus -> Value.Number (l +. r)
    | Token_kind.Minus -> Value.Number (l -. r)
    | Token_kind.Star -> Value.Number (l *. r)
    | Token_kind.Slash -> Value.Number (l /. r)
    | Token_kind.Greater -> Value.Bool (l > r)
    | Token_kind.Greater_equal -> Value.Bool (l >= r)
    | Token_kind.Less -> Value.Bool (l < r)
    | Token_kind.Less_equal -> Value.Bool (l <= r)
    | _ -> failwith "not implemented" )
  | Value.String l, t, Value.String r -> (
    match t with
    | Token_kind.Plus -> Value.String (l ^ r)
    | _ -> failwith "not implemented" )
  | _ -> failwith "not implemented"
