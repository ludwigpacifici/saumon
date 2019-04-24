open Omnipresent
open Token

(* Parse  rules following the pattern:
 * rule -> k ( [infix0; ...; infixN] k )* ;
 *)
let consume_one_or_many k (infixes : Token_kind.t list) ts =
  let open Base in
  let rec aux left = function
    | ({kind; _} as infix) :: ts
      when List.exists infixes ~f:(fun k -> Token_kind.equal kind k) ->
        let right, ts = k ts in
        aux (Ast.Binary (left, infix, right)) ts
    | ts -> (left, ts)
  in
  k ts ||> aux

let rec expression ts = equality ts

and equality ts =
  consume_one_or_many comparison
    [Token_kind.Equal_equal; Token_kind.Bang_equal]
    ts

and comparison ts =
  consume_one_or_many addition
    [ Token_kind.Less
    ; Token_kind.Less_equal
    ; Token_kind.Greater
    ; Token_kind.Greater_equal ]
    ts

and addition ts =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus] ts

and multiplication ts =
  consume_one_or_many unary [Token_kind.Star; Token_kind.Slash] ts

and unary = function
  | ({kind = Token_kind.Bang; _} as prefix) :: ts
   |({kind = Token_kind.Minus; _} as prefix) :: ts ->
      let e, ts = unary ts in
      (Ast.Unary (prefix, e), ts)
  | ts -> primary ts

and primary = function
  | {kind = Token_kind.Number x; _} :: ts -> (Ast.Literal (Ast.Number x), ts)
  | {kind = Token_kind.String x; _} :: ts -> (Ast.Literal (Ast.String x), ts)
  | {kind = Token_kind.True; _} :: ts -> (Ast.Literal (Ast.Bool true), ts)
  | {kind = Token_kind.False; _} :: ts -> (Ast.Literal (Ast.Bool false), ts)
  | {kind = Token_kind.Nil; _} :: ts -> (Ast.Literal Ast.Nil, ts)
  | ({kind = Token_kind.Left_paren; _} as left_paren) :: ts -> (
      let expr, ts = expression ts in
      match ts with
      | ({kind = Token_kind.Right_paren; _} as right_paren) :: ts ->
          (Ast.Grouping (left_paren, expr, right_paren), ts)
      | _ -> failwith "Expected a right paren to close expression" )
  | {kind = k; _} :: _ ->
      failwith ("Unsupported token_kind: " ^ Token_kind.show k)
  | [] -> failwith "Empty token list"

let parse = expression >> fst
