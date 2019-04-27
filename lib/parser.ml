open Base
open Omnipresent
open Token

(* Parse  rules following the pattern:
 * rule -> k ( [infix0; ...; infixN] k )* ;
 *)
let consume_one_or_many k infixes ts =
  let open Base in
  let rec aux left = function
    | ({kind; _} as infix) :: ts
      when List.exists infixes ~f:(fun k -> Token_kind.equal kind k) ->
        k ts
        |> Result.bind ~f:(fun (right, ts) ->
               aux (Ast.Binary (left, infix, right)) ts )
    | ts -> Ok (left, ts)
  in
  k ts |> Result.bind ~f:(fun (l, ts) -> aux l ts)

let rec expression ts =
  equality ts
  |> Result.map_error ~f:(fun (msg, t) ->
         ("Cannot read expression" :: msg |> List.rev, t) )

and equality ts =
  consume_one_or_many comparison
    [Token_kind.Equal_equal; Token_kind.Bang_equal]
    ts
  |> Result.map_error ~f:(fun (msg, t) -> ("Cannot read equality" :: msg, t))

and comparison ts =
  consume_one_or_many addition
    [ Token_kind.Less
    ; Token_kind.Less_equal
    ; Token_kind.Greater
    ; Token_kind.Greater_equal ]
    ts
  |> Result.map_error ~f:(fun (msg, t) -> ("Cannot read comparison" :: msg, t))

and addition ts =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus] ts
  |> Result.map_error ~f:(fun (msg, t) -> ("Cannot read addition" :: msg, t))

and multiplication ts =
  consume_one_or_many unary [Token_kind.Star; Token_kind.Slash] ts
  |> Result.map_error ~f:(fun (msg, t) ->
         ("Cannot read multiplication" :: msg, t) )

and unary = function
  | ({kind = Token_kind.Bang; _} as prefix) :: ts
   |({kind = Token_kind.Minus; _} as prefix) :: ts ->
      Result.map (unary ts) ~f:(fun (e, ts) -> (Ast.Unary (prefix, e), ts))
  | ts -> primary ts

and primary = function
  | {kind = Token_kind.Number x; _} :: ts -> Ok (Ast.Literal (Ast.Number x), ts)
  | {kind = Token_kind.String x; _} :: ts -> Ok (Ast.Literal (Ast.String x), ts)
  | {kind = Token_kind.True; _} :: ts -> Ok (Ast.Literal (Ast.Bool true), ts)
  | {kind = Token_kind.False; _} :: ts -> Ok (Ast.Literal (Ast.Bool false), ts)
  | {kind = Token_kind.Nil; _} :: ts -> Ok (Ast.Literal Ast.Nil, ts)
  | ({kind = Token_kind.Left_paren; _} as left_paren) :: ts ->
      Result.bind (expression ts) ~f:(fun (e, ts) ->
          match ts with
          | ({kind = Token_kind.Right_paren; _} as right_paren) :: ts ->
              Ok (Ast.Grouping (left_paren, e, right_paren), ts)
          | t :: _ ->
              Error (["Expected a right paren to close expression"], Some t)
          | [] ->
              Error
                ( [ "Expected a right paren to close expression but none \
                     available" ]
                , None ) )
  | t :: _ -> Error (["Unexpected token"], Some t)
  | [] -> Error (["No token available to parse an expression"], None)

let parse = expression >> Result.map ~f:fst
