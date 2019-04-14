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

let%test "Expression is number" =
  let inner = 42. in
  primary [Token.of_token_kind ~kind:(Token_kind.Number inner)]
  = (Ast.Literal (Ast.Number inner), [])

let%test "Expression is string" =
  let inner = "hello" in
  primary [Token.of_token_kind ~kind:(Token_kind.String inner)]
  = (Ast.Literal (Ast.String inner), [])

let%test "Expression is true bool" =
  primary [Token.of_token_kind ~kind:Token_kind.True]
  = (Ast.Literal (Ast.Bool true), [])

let%test "Expression is false bool" =
  primary [Token.of_token_kind ~kind:Token_kind.False]
  = (Ast.Literal (Ast.Bool false), [])

let%test "Expression is nil" =
  primary [Token.of_token_kind ~kind:Token_kind.Nil] = (Ast.Literal Ast.Nil, [])

let rec unary = function
  | ({kind = Token_kind.Bang; _} as prefix) :: ts
   |({kind = Token_kind.Minus; _} as prefix) :: ts ->
      let e, ts = unary ts in
      (Ast.Unary (prefix, e), ts)
  | ts -> primary ts

let%test "Expression is bang unary" =
  let bang = Token.of_token_kind ~kind:Token_kind.Bang in
  unary [bang; Token.of_token_kind ~kind:Token_kind.True]
  = (Ast.Unary (bang, Ast.Literal (Ast.Bool true)), [])

let%test "Expression is minus number" =
  let minus = Token.of_token_kind ~kind:Token_kind.Minus in
  let n = 42. in
  unary [minus; Token.of_token_kind ~kind:(Token_kind.Number n)]
  = (Ast.Unary (minus, Ast.Literal (Ast.Number n)), [])

let%test "Expression is actually primary" =
  unary [Token.of_token_kind ~kind:Token_kind.True]
  = (Ast.Literal (Ast.Bool true), [])

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

let%test "multiplication is actually one literal" =
  let n = 42. in
  multiplication [Token.of_token_kind ~kind:(Token_kind.Number n)]
  = (Ast.Literal (Ast.Number n), [])

let%test "multiplication of two numbers" =
  let n = 42. in
  let infix = Token.of_token_kind ~kind:Token_kind.Star in
  multiplication
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  = ( Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
    , [] )

let%test "division of three numbers" =
  let infix = Token.of_token_kind ~kind:Token_kind.Slash in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual =
    multiplication [number 1.; infix; number 2.; infix; number 3.]
  in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)), []) in
  actual = expect

let addition =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus]

let%test "addition is actually one literal" =
  let n = 42. in
  addition [Token.of_token_kind ~kind:(Token_kind.Number n)]
  = (Ast.Literal (Ast.Number n), [])

let%test "addition of two numbers" =
  let n = 42. in
  let infix = Token.of_token_kind ~kind:Token_kind.Plus in
  addition
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  = ( Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
    , [] )

let%test "substract three numbers" =
  let infix = Token.of_token_kind ~kind:Token_kind.Minus in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = addition [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)), []) in
  actual = expect

let comparison =
  consume_one_or_many addition
    [ Token_kind.Less
    ; Token_kind.Less_equal
    ; Token_kind.Greater
    ; Token_kind.Greater_equal ]

let%test "comparison is actually one literal" =
  let n = 42. in
  comparison [Token.of_token_kind ~kind:(Token_kind.Number n)]
  = (Ast.Literal (Ast.Number n), [])

let%test "comparison of two numbers" =
  let n = 42. in
  let infix = Token.of_token_kind ~kind:Token_kind.Greater in
  comparison
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  = ( Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
    , [] )

let%test "comparison three numbers" =
  let infix = Token.of_token_kind ~kind:Token_kind.Less_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = comparison [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)), []) in
  actual = expect

let equality =
  consume_one_or_many comparison [Token_kind.Equal_equal; Token_kind.Bang_equal]

let%test "equal_equal is actually one literal" =
  let n = 42. in
  equality [Token.of_token_kind ~kind:(Token_kind.Number n)]
  = (Ast.Literal (Ast.Number n), [])

let%test "equality of two numbers" =
  let n = 42. in
  let infix = Token.of_token_kind ~kind:Token_kind.Equal_equal in
  equality
    [ Token.of_token_kind ~kind:(Token_kind.Number n)
    ; infix
    ; Token.of_token_kind ~kind:(Token_kind.Number n) ]
  = ( Ast.Binary (Ast.Literal (Ast.Number n), infix, Ast.Literal (Ast.Number n))
    , [] )

let%test "equality three numbers" =
  let infix = Token.of_token_kind ~kind:Token_kind.Bang_equal in
  let number n = Token.of_token_kind ~kind:(Token_kind.Number n) in
  let actual = equality [number 1.; infix; number 2.; infix; number 3.] in
  let expect =
    Ast.Binary (Ast.Literal (Ast.Number 1.), infix, Ast.Literal (Ast.Number 2.))
  in
  let expect = (Ast.Binary (expect, infix, Ast.Literal (Ast.Number 3.)), []) in
  actual = expect

let equality (ts : Token.t list) =
  let expr, ts = equality ts in
  match ts with
  | ({kind = Token_kind.Equal_equal; _} as t) :: _
   |({kind = Token_kind.Bang_equal; _} as t) :: _ ->
      Ast.Binary (expr, t, expr)
  | _ -> expr

let parse ts = equality ts
