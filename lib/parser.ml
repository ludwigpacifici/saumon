open Base
open Token

let ( let* ) x f = Result.bind ~f x

let ( let+ ) x f = Result.map ~f x

(* Parse rules following pattern: rule -> k ( [infix0; ...; infixN] k )* ; *)
let consume_one_or_many k (infixes : Token_kind.t list) (ts : Token.t list) =
  let rec aux left = function
    | ({kind; _} as infix) :: ts
      when List.exists infixes ~f:(fun k -> Token_kind.equal kind k) ->
        let* right, ts = k ts in
        aux (Ast.Binary (left, infix, right)) ts
    | ts -> Ok (left, ts)
  in
  let* l, ts = k ts in
  aux l ts

let rec expression (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  assignment ts

and assignment (ts : Token.t list) :
    (Ast.expression * Token.t list, string) Result.t =
  match ts with
  | {kind = Token_kind.Identifier identifier; _}
    :: ({kind = Token_kind.Equal; _} as equal) :: ts ->
      let+ expr, ts = expression ts in
      (Ast.Assignment (Ast.Identifier identifier, equal, expr), ts)
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
      let+ e, ts = unary ts in
      (Ast.Unary (prefix, e), ts)
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
  | ({kind = Token_kind.Left_paren; _} as left_paren) :: ts -> (
      let* env, ts = expression ts in
      match ts with
      | ({kind = Token_kind.Right_paren; _} as right_paren) :: ts ->
          Ok (Ast.Grouping (left_paren, env, right_paren), ts)
      | _ -> Error "Expected ')' to close expression" )
  | t :: _ -> Error ("Unexpected token: " ^ Token.show t)
  | [] -> Error "No token available to parse an expression"

let pop_token ~(t : Token_kind.t) = function
  | ({kind = token_kind; _} as token) :: ts when Token_kind.equal token_kind t
    ->
      Ok (token, ts)
  | _ -> Error "Expected an expression before ';'"

(* Parse rule pattern: rule -> expression * ";" *)
let expression_statement (ts : Token.t list) :
    (Ast.expression_statement * Token.t list, string) Result.t =
  let* e, ts = expression ts in
  let+ _semicolon, ts = pop_token ~t:Token_kind.Semicolon ts in
  (e, ts)

let variable_declaration (ts : Token.t list) :
    (Ast.variable_declaration * Token.t list, string) Result.t =
  match ts with
  | ({kind = Token_kind.Var; _} as var) :: ts -> (
      let* exp, ts = primary ts in
      match exp with
      | Ast.Literal (Ast.Identifier _ as identifier) -> (
        match ts with
        | ({kind = Token_kind.Equal; _} as equal) :: ts -> (
            let* exp, ts = expression ts in
            match (exp, ts) with
            | exp, ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
                Ok ((var, identifier, Some (equal, exp), semicolon), ts)
            | _ ->
                Error
                  "After '=' expected to get an expression followed by a \
                   semicolon" )
        | ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
            Ok ((var, identifier, None, semicolon), ts)
        | _ -> Error "';' or '=' is expected to declare a variable." )
      | _ ->
          Error
            "A literal identifier after 'var' is expected to declare a \
             variable name" )
  | _ -> Error "A variable declaration must start with 'var' keyword"

let rec block (ts : Token.t list) : (Ast.block * Token.t list, string) Result.t
    =
  let rec aux acc = function
    | {kind = Token_kind.Right_brace; _} :: ts -> Ok (List.rev acc, ts)
    | [{kind = Token_kind.Eof; _}] | [] -> Error "Expected closing brace '}'."
    | ts ->
        let* d, ts = declaration ts in
        aux (d :: acc) ts
  in
  aux [] ts

and statement (ts : Token.t list) :
    (Ast.statement * Token.t list, string) Result.t =
  match ts with
  | ({kind = Token_kind.If; _} as if_token)
    :: ({kind = Token_kind.Left_paren; _} as left_paren) :: ts ->
      let+ parsed_if, ts = if_statement if_token left_paren ts in
      (Ast.If_statement parsed_if, ts)
  | {kind = Token_kind.For; _} :: _ ->
      let+ parsed_for, ts = for_statement ts in
      (* For loop parsed and de-sugared as mostly a while loop *)
      (Ast.Block parsed_for, ts)
  | {kind = Token_kind.Left_brace; _} :: ts ->
      let+ block, ts = block ts in
      (Ast.Block block, ts)
  | {kind = Token_kind.Print; _} :: ts ->
      let+ e, ts = expression_statement ts in
      (Ast.Print_statement e, ts)
  | {kind = Token_kind.While; _} :: {kind = Token_kind.Left_paren; _} :: ts ->
      let+ parsed_while, ts = while_statement ts in
      (Ast.While_statement parsed_while, ts)
  | ts ->
      let+ statement, ts = expression_statement ts in
      (Ast.Expression_statement statement, ts)

(* TODO: Parser helper should just take the Token.t list otherwise it is harder
   to re-use helper. *)
and if_statement (if_token : Token.t) (left_paren : Token.t) (ts : Token.t list)
    : (Ast.if_statement * Token.t list, string) Result.t =
  let* condition, ts = expression ts in
  match ts with
  | ({kind = Token_kind.Right_paren; _} as right_paren) :: ts -> (
      let* if_body, ts = statement ts in
      match ts with
      | ({kind = Token_kind.Else; _} as else_token) :: ts ->
          let* else_body, ts = statement ts in
          let parsed_if_else =
            ( if_token
            , left_paren
            , condition
            , right_paren
            , if_body
            , Some (else_token, else_body) )
          in
          Ok (parsed_if_else, ts)
      | ts ->
          let parsed_if =
            (if_token, left_paren, condition, right_paren, if_body, None)
          in
          Ok (parsed_if, ts) )
  | _ -> Error "Expected ')' after if condition."

(* A for loop does not have an AST representation because it is syntax sugar for
   a while loop. *)
and for_statement (ts : Token.t list) : (_ * Token.t list, string) Result.t =
  (* Parse rule pattern: rule -> f? delimiter_token *)
  let try_parse f (delimiter_token : Token_kind.t) = function
    | t :: ts when Token_kind.equal t.kind delimiter_token -> Ok (None, ts)
    | ts ->
        let* x, ts = f ts in
        let* _, ts = pop_token ~t:delimiter_token ts in
        Ok (Some x, ts)
  in
  match ts with
  | {kind = Token_kind.For; _} :: {kind = Token_kind.Left_paren; _} :: ts ->
      let* for_initializer, ts =
        match variable_declaration ts with
        | Ok (var_decl, ts) -> Ok (Either.First var_decl, ts)
        | Error _ -> (
          match expression_statement ts with
          | Ok (expr_stmt, ts) -> Ok (Either.Second expr_stmt, ts)
          | Error _ ->
              Error
                "For initializer clause should be variable declaration or an \
                 expression statement" )
      in
      let* for_condition, ts = try_parse expression Token_kind.Semicolon ts in
      let* for_increment, ts = try_parse expression Token_kind.Right_paren ts in
      let* for_body, ts = statement ts in
      (* `for (initializer ; condition ; increment ) for_body`
       * is equivalent to:
       * `{ initializer; while (condition) { for_body ; increment } }` *)
      let while_condition =
        Option.value ~default:(Ast.Literal (Ast.Bool true)) for_condition
      in
      let for_body = Ast.Statement for_body in
      let for_increment =
        for_increment
        |> Option.map ~f:List.return
        |> Option.value ~default:[]
        |> List.map ~f:(fun e -> Ast.Expression_statement e)
        |> List.map ~f:(fun e -> Ast.Statement e)
      in
      let while_body = Ast.Block (for_body :: for_increment) in
      let for_as_while = Ast.While_statement (while_condition, while_body) in
      let for_initializer =
        match for_initializer with
        | First var_decl -> Ast.Variable_declaration var_decl
        | Second expr_stmt -> Ast.Statement (Ast.Expression_statement expr_stmt)
      in
      let for_rewritten = [for_initializer; Ast.Statement for_as_while] in
      Ok (for_rewritten, ts)
  | _ -> Error "Cannot parse a for loop. Missing for token."

and while_statement (ts : Token.t list) :
    (Ast.while_statement * Token.t list, string) Result.t =
  let* condition, ts = expression ts in
  match ts with
  | {kind = Token_kind.Right_paren; _} :: ts ->
      let* while_body, ts = statement ts in
      let parsed_while = (condition, while_body) in
      Ok (parsed_while, ts)
  | _ -> Error "Expected ')' after if condition."

and declaration (ts : Token.t list) :
    (Ast.declaration * Token.t list, string) Result.t =
  match ts with
  | {kind = Token_kind.Var; _} :: _ ->
      let+ var_decl, ts = variable_declaration ts in
      (Ast.Variable_declaration var_decl, ts)
  | ts ->
      let+ s, ts = statement ts in
      (Ast.Statement s, ts)

let rec loop (acc : Ast.declaration list) (ts : Token.t list) :
    (Ast.declaration list, string) Result.t =
  match ts with
  | [] | [{kind = Token_kind.Eof; _}] -> Ok acc
  | ts ->
      let* x, ts = declaration ts in
      loop (x :: acc) ts

let parse (ts : Token.t list) : (Ast.Program.t, string) Result.t =
  let+ x = loop [] ts in
  List.rev x |> Ast.Program.return
