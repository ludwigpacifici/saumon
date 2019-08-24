open Base
open Token

(* Parse rules following pattern:
 * rule -> k ( [infix0; ...; infixN] k )* ;
 *)
let consume_one_or_many k (infixes : Token_kind.t list) (ts : Token.t list) =
  let rec aux left = function
    | ({kind; _} as infix) :: ts
      when List.exists infixes ~f:(fun k -> Token_kind.equal kind k) ->
        k ts
        |> Result.bind ~f:(fun (right, ts) ->
               aux (Ast.Binary (left, infix, right)) ts)
    | ts -> Ok (left, ts)
  in
  k ts |> Result.bind ~f:(fun (l, ts) -> aux l ts)

(* A valid Ast part and the rest of the token list to be parsed *)
type 'ast ok = 'ast * Token.t list

(* A list of error that are maybe related to a Token*)
type error = string list * Token.t option

let rec expression (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  equality ts
  |> Result.map_error ~f:(fun (msg, t) ->
         ("Cannot read expression" :: msg |> List.rev, t))

and equality (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  consume_one_or_many comparison
    [Token_kind.Equal_equal; Token_kind.Bang_equal]
    ts
  |> Result.map_error ~f:(fun (msg, t) -> ("Cannot read equality" :: msg, t))

and comparison (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  consume_one_or_many addition
    [ Token_kind.Less
    ; Token_kind.Less_equal
    ; Token_kind.Greater
    ; Token_kind.Greater_equal ]
    ts
  |> Result.map_error ~f:(fun (msg, t) -> ("Cannot read comparison" :: msg, t))

and addition (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus] ts
  |> Result.map_error ~f:(fun (msg, t) -> ("Cannot read addition" :: msg, t))

and multiplication (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  consume_one_or_many unary [Token_kind.Star; Token_kind.Slash] ts
  |> Result.map_error ~f:(fun (msg, t) ->
         ("Cannot read multiplication" :: msg, t))

and unary (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  match ts with
  | ({kind = Token_kind.Bang; _} as prefix) :: ts
   |({kind = Token_kind.Minus; _} as prefix) :: ts ->
      Result.map (unary ts) ~f:(fun (e, ts) -> (Ast.Unary (prefix, e), ts))
  | ts -> primary ts

and primary (ts : Token.t list) : (Ast.expression ok, error) Result.t =
  match ts with
  | {kind = Token_kind.Number x; _} :: ts -> Ok (Ast.Literal (Ast.Number x), ts)
  | {kind = Token_kind.String x; _} :: ts -> Ok (Ast.Literal (Ast.String x), ts)
  | {kind = Token_kind.True; _} :: ts -> Ok (Ast.Literal (Ast.Bool true), ts)
  | {kind = Token_kind.False; _} :: ts -> Ok (Ast.Literal (Ast.Bool false), ts)
  | {kind = Token_kind.Nil; _} :: ts -> Ok (Ast.Literal Ast.Nil, ts)
  | {kind = Token_kind.Identifier x; _} :: ts ->
      Ok (Ast.Literal (Ast.Identifier x), ts)
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
                , None ))
  | t :: _ -> Error (["Unexpected token"], Some t)
  | [] -> Error (["No token available to parse an expression"], None)

(* Parse rules following pattern: rule -> expression * ";" *)
let expression_statement (ts : Token.t list) :
    (Ast.expression_statement ok, error) Result.t =
  let expect_semicolon = function
    | ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
        Ok (semicolon, ts)
    | t :: _ -> Error (["Expected a semicolon after parsed expression"], Some t)
    | [] ->
        Error
          ( ["Expected a semicolon after parsed expression, but no token found"]
          , None )
  in
  expression ts
  |> Result.bind ~f:(fun (e, ts) ->
         expect_semicolon ts
         |> Result.map ~f:(fun (semicolon, ts) -> ((e, semicolon), ts)))

let statement (ts : Token.t list) : (Ast.statement option ok, error) Result.t =
  match ts with
  | {kind = Token_kind.Semicolon; _} :: ts -> Ok (None, ts)
  | ({kind = Token_kind.Print; _} as print) :: ts ->
      expression_statement ts
      |> Result.map ~f:(fun ((e, semicolon), ts) ->
             (Some (Ast.Print_statement (print, e, semicolon)), ts))
  | ts ->
      expression_statement ts
      |> Result.map ~f:(fun ((e, semicolon), ts) ->
             (Some (Ast.Expression_statement (e, semicolon)), ts))

let declaration (ts : Token.t list) :
    (Ast.declaration option ok, error) Result.t =
  match ts with
  | [] | [{kind = Token_kind.Eof; _}] -> Ok (None, [])
  | ({kind = Token_kind.Var; _} as var) :: ts ->
      primary ts
      |> Result.bind ~f:(fun (e, ts) ->
             match e with
             | Ast.Literal (Ast.Identifier raw_identifier as identifier) -> (
               match ts with
               | ({kind = Token_kind.Equal; _} as equal) :: ts ->
                   expression ts
                   |> Result.bind ~f:(function
                        | ( value
                          , ({kind = Token_kind.Semicolon; _} as semicolon)
                            :: ts ) ->
                            Ok
                              ( Some
                                  (Ast.Variable_declaration
                                     ( var
                                     , identifier
                                     , Some (equal, value)
                                     , semicolon ))
                              , ts )
                        | _ ->
                            Error
                              ( [ "After '=' expected to get an expression \
                                   followed by a semicolon" ]
                              , Some equal ))
               | ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
                   Ok
                     ( Some
                         (Ast.Variable_declaration
                            (var, identifier, None, semicolon))
                     , ts )
               | token :: _ ->
                   Error
                     ( [ "';' or '=' is expected to declare a variable \
                          without or with a value after: " ^ raw_identifier ]
                     , Some token )
               | [] ->
                   Error
                     ( [ "Unexpected end of tokens after identifier: "
                         ^ raw_identifier ]
                     , Some var ) )
             | _ ->
                 Error
                   ( [ "A literal identifier after 'var' is expected to \
                        declare a variable name" ]
                   , Some var ))
  | ts ->
      statement ts
      |> Result.map ~f:(fun (s, ts) ->
             (Option.map ~f:(fun s -> Ast.Statement s) s, ts))

let rec loop (acc : Ast.declaration list) (ts : Token.t list) :
    (Ast.declaration list, error) Result.t =
  match ts with
  | [] -> Ok acc
  | ts ->
      declaration ts
      |> Result.bind ~f:(fun (x, ts) ->
             match x with Some x -> loop (x :: acc) ts | None -> loop acc ts)

let parse (ts : Token.t list) : (Ast.Program.t, error) Result.t =
  let open Omnipresent in
  loop [] ts |> Result.map ~f:(List.rev >> Ast.Program.return)
