open Omnipresent
open Token

let primary = function
  | {kind = Token_kind.Number x; _} :: ts -> (Ast.Literal (Ast.Number x), ts)
  | {kind = Token_kind.String x; _} :: ts -> (Ast.Literal (Ast.String x), ts)
  | {kind = Token_kind.True; _} :: ts -> (Ast.Literal (Ast.Bool true), ts)
  | {kind = Token_kind.False; _} :: ts -> (Ast.Literal (Ast.Bool false), ts)
  | {kind = Token_kind.Nil; _} :: ts -> (Ast.Literal Ast.Nil, ts)
  | {kind = k; _} :: _ ->
      failwith ("Unsupported token_kind" ^ Token_kind.to_string k)
  | [] -> failwith "Empty token list"

let rec unary = function
  | ({kind = Token_kind.Bang; _} as prefix) :: ts
   |({kind = Token_kind.Minus; _} as prefix) :: ts ->
      let e, ts = unary ts in
      (Ast.Unary (prefix, e), ts)
  | ts -> primary ts

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

let multiplication =
  consume_one_or_many unary [Token_kind.Star; Token_kind.Slash]

let addition =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus]

let comparison =
  consume_one_or_many addition
    [ Token_kind.Less
    ; Token_kind.Less_equal
    ; Token_kind.Greater
    ; Token_kind.Greater_equal ]

let equality =
  consume_one_or_many comparison [Token_kind.Equal_equal; Token_kind.Bang_equal]

let parse = equality >> fst
