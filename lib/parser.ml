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

let rec expression (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  assignment ts

and assignment (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  match ts with
  | {kind = Token_kind.Identifier identifier; _}
    :: ({kind = Token_kind.Equal; _} as equal) :: ts ->
      expression ts
      |> Result.map ~f:(fun (expr, ts) ->
             (Ast.Assignment (Ast.Identifier identifier, equal, expr), ts))
  | ts -> logic_or ts

and logic_or (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  consume_one_or_many logic_and [Token_kind.Or] ts

and logic_and (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  consume_one_or_many equality [Token_kind.And] ts

and equality (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  consume_one_or_many comparison
    [Token_kind.Equal_equal; Token_kind.Bang_equal]
    ts

and comparison (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  consume_one_or_many addition
    [ Token_kind.Less
    ; Token_kind.Less_equal
    ; Token_kind.Greater
    ; Token_kind.Greater_equal ]
    ts

and addition (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus] ts

and multiplication (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  consume_one_or_many unary [Token_kind.Star; Token_kind.Slash] ts

and unary (ts : Token.t list) : (Ast.expression * Token.t list, string) Result.t
    =
  match ts with
  | ({kind = Token_kind.Bang; _} as prefix) :: ts
   |({kind = Token_kind.Minus; _} as prefix) :: ts ->
      Result.map (unary ts) ~f:(fun (e, ts) -> (Ast.Unary (prefix, e), ts))
  | ts -> primary ts

and primary (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
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
          | _ -> Error "Expected ')' to close expression")
  | t :: _ -> Error ("Unexpected token: " ^ Token.show t)
  | [] -> Error "No token available to parse an expression"

(* Parse rules following pattern: rule -> expression * ";" *)
let expression_statement (ts : Token.t list) :
    (Ast.expression_statement * Token.t list, string) Result.t =
  let expect_semicolon = function
    | ({kind = Token_kind.Semicolon; _} as semicolon) :: ts -> Ok (semicolon, ts)
    | _ -> Error "Expected an expression before ';'"
  in
  expression ts
  |> Result.bind ~f:(fun (e, ts) ->
         expect_semicolon ts
         |> Result.map ~f:(fun (semicolon, ts) -> ((e, semicolon), ts)))

let rec block (open_brace : Token.t) (ts : Token.t list) :
    (Ast.block * Token.t list, string) Result.t =
  let rec aux acc = function
    | ({kind = Token_kind.Right_brace; _} as close_brace) :: ts ->
        Ok ((open_brace, List.rev acc, close_brace), ts)
    | [{kind = Token_kind.Eof; _}] | [] -> Error "Expected closing brace '}'."
    | ts -> declaration ts |> Result.bind ~f:(fun (d, ts) -> aux (d :: acc) ts)
  in
  aux [] ts

and statement (ts : Token.t list) :
    (Ast.statement * Token.t list, string) Result.t =
  match ts with
  | ({kind = Token_kind.Print; _} as print) :: ts ->
      expression_statement ts
      |> Result.map ~f:(fun ((e, semicolon), ts) ->
             (Ast.Print_statement (print, e, semicolon), ts))
  | ({kind = Token_kind.Left_brace; _} as new_block) :: ts ->
      block new_block ts
      |> Result.map ~f:(fun (block, ts) -> (Ast.Block block, ts))
  | ({kind = Token_kind.If; _} as if_token)
    :: ({kind = Token_kind.Left_paren; _} as left_paren) :: ts ->
      if_statement if_token left_paren ts
      |> Result.map ~f:(fun (parsed_if, ts) -> (Ast.If_statement parsed_if, ts))
  | ts ->
      expression_statement ts
      |> Result.map ~f:(fun (statement, ts) ->
             (Ast.Expression_statement statement, ts))

and if_statement (if_token : Token.t) (left_paren : Token.t) (ts : Token.t list)
    : (Ast.if_statement * Token.t list, string) Result.t =
  expression ts
  |> Result.bind ~f:(fun (condition, ts) ->
         match ts with
         | ({kind = Token_kind.Right_paren; _} as right_paren) :: ts ->
             statement ts
             |> Result.bind ~f:(fun (if_body, ts) ->
                    match ts with
                    | ({kind = Token_kind.Else; _} as else_token) :: ts ->
                        statement ts
                        |> Result.bind ~f:(fun (else_body, ts) ->
                               let parsed_if_else =
                                 ( if_token
                                 , left_paren
                                 , condition
                                 , right_paren
                                 , if_body
                                 , Some (else_token, else_body) )
                               in
                               Ok (parsed_if_else, ts))
                    | ts ->
                        let parsed_if =
                          ( if_token
                          , left_paren
                          , condition
                          , right_paren
                          , if_body
                          , None )
                        in
                        Ok (parsed_if, ts))
         | _ -> Error "Expected ')' after if condition.")

and declaration (ts : Token.t list) :
    (Ast.declaration * Token.t list, string) Result.t =
  match ts with
  | ({kind = Token_kind.Var; _} as var) :: ts ->
      primary ts
      |> Result.bind ~f:(fun (e, ts) ->
             match e with
             | Ast.Literal (Ast.Identifier _ as identifier) -> (
               match ts with
               | ({kind = Token_kind.Equal; _} as equal) :: ts ->
                   expression ts
                   |> Result.bind ~f:(function
                        | ( value
                          , ({kind = Token_kind.Semicolon; _} as semicolon)
                            :: ts ) ->
                            Ok
                              ( Ast.Variable_declaration
                                  ( var
                                  , identifier
                                  , Some (equal, value)
                                  , semicolon )
                              , ts )
                        | _ ->
                            Error
                              "After '=' expected to get an expression \
                               followed by a semicolon")
               | ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
                   Ok
                     ( Ast.Variable_declaration
                         (var, identifier, None, semicolon)
                     , ts )
               | _ -> Error "';' or '=' is expected to declare a variable." )
             | _ ->
                 Error
                   "A literal identifier after 'var' is expected to declare a \
                    variable name")
  | ts -> statement ts |> Result.map ~f:(fun (s, ts) -> (Ast.Statement s, ts))

let rec loop (acc : Ast.declaration list) (ts : Token.t list) :
    (Ast.declaration list, string) Result.t =
  match ts with
  | [] | [{kind = Token_kind.Eof; _}] -> Ok acc
  | ts -> declaration ts |> Result.bind ~f:(fun (x, ts) -> loop (x :: acc) ts)

let parse (ts : Token.t list) : (Ast.Program.t, string) Result.t =
  let open Omnipresent in
  loop [] ts |> Result.map ~f:(List.rev >> Ast.Program.return)
