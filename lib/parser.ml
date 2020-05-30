open Base
open Token

let ( let* ) x f = Result.bind ~f x

let ( let+ ) x f = Result.map ~f x

(* Parse rules following pattern: rule -> k ( [infix0; ...; infixN] k )* ; *)
let consume_one_or_many k (infixes : Token_kind.t list) (ts : Token.t list) =
  let rec aux left_expr = function
    | ({kind; _} as infix) :: ts when List.exists infixes ~f:(fun k -> Token_kind.equal kind k) ->
        let* right_expr, ts = k ts in
        aux (Ast.Binary {left_expr; operator = infix; right_expr}) ts
    | ts -> Ok (left_expr, ts)
  in
  let* l, ts = k ts in
  aux l ts

(* Pop an expected token. *)
let pop_token ~(t : Token_kind.t) = function
  | ({kind = token_kind; _} as token) :: ts when Token_kind.equal token_kind t -> Ok (token, ts)
  | _ -> Error "Expected an expression before ';'"

(* Parse rule pattern: rule -> f? delimiter_token *)
let try_parse f (delimiter_token : Token_kind.t) = function
  | t :: ts when Token_kind.equal t.kind delimiter_token -> Ok (None, ts)
  | ts ->
      let* x, ts = f ts in
      let+ _, ts = pop_token ~t:delimiter_token ts in
      (Some x, ts)

(* Represents a partial output from one of the parsing functions. If parsing is succesful, it represents an AST and the
   remaining token to be parsed. Otherwise, it is an error message. *)
type 'ast partial = ('ast * Token.t list, string) Result.t

let rec expression (ts : Token.t list) : Ast.expression partial = assignment ts

and assignment : Token.t list -> Ast.expression partial = function
  | {kind = Token_kind.Identifier identifier; _} :: ({kind = Token_kind.Equal; _} as equal) :: ts ->
      let+ expr, ts = expression ts in
      (Ast.Assignment {identifier = Ast.Identifier identifier; equal; expr}, ts)
  | ts -> logic_or ts

and logic_or (ts : Token.t list) : Ast.expression partial = consume_one_or_many logic_and [Token_kind.Or] ts

and logic_and (ts : Token.t list) : Ast.expression partial = consume_one_or_many equality [Token_kind.And] ts

and equality (ts : Token.t list) : Ast.expression partial =
  consume_one_or_many comparison [Token_kind.Equal_equal; Token_kind.Bang_equal] ts

and comparison (ts : Token.t list) : Ast.expression partial =
  consume_one_or_many addition [Token_kind.Less; Token_kind.Less_equal; Token_kind.Greater; Token_kind.Greater_equal] ts

and addition (ts : Token.t list) : Ast.expression partial =
  consume_one_or_many multiplication [Token_kind.Plus; Token_kind.Minus] ts

and multiplication (ts : Token.t list) : Ast.expression partial =
  consume_one_or_many unary [Token_kind.Star; Token_kind.Slash] ts

and unary : Token.t list -> Ast.expression partial = function
  | ({kind = Token_kind.Bang; _} as operator) :: ts | ({kind = Token_kind.Minus; _} as operator) :: ts ->
      let+ expr, ts = unary ts in
      (Ast.Unary {operator; expr}, ts)
  | ts -> primary ts

and primary : Token.t list -> Ast.expression partial = function
  | {kind = Token_kind.Number x; _} :: ts -> Ok (Ast.Literal (Ast.Number x), ts)
  | {kind = Token_kind.String x; _} :: ts -> Ok (Ast.Literal (Ast.String x), ts)
  | {kind = Token_kind.True; _} :: ts -> Ok (Ast.Literal (Ast.Bool true), ts)
  | {kind = Token_kind.False; _} :: ts -> Ok (Ast.Literal (Ast.Bool false), ts)
  | {kind = Token_kind.Nil; _} :: ts -> Ok (Ast.Literal Ast.Nil, ts)
  | {kind = Token_kind.Identifier x; _} :: ts -> Ok (Ast.Literal (Ast.Identifier x), ts)
  | ({kind = Token_kind.Left_paren; _} as left_paren) :: ts -> (
      let* expr, ts = expression ts in
      match ts with
      | ({kind = Token_kind.Right_paren; _} as right_paren) :: ts ->
          Ok (Ast.Grouping {left_paren; expr; right_paren}, ts)
      | _ -> Error "Expected ')' to close expression" )
  | t :: _ -> Error ("Unexpected token: " ^ Token.show t)
  | [] -> Error "No token available to parse an expression"

(* Parse rule pattern: rule -> expression * ";" *)
let expression_statement (ts : Token.t list) : Ast.expression partial =
  let* expr, ts = expression ts in
  let+ _semicolon, ts = pop_token ~t:Token_kind.Semicolon ts in
  (expr, ts)

let variable_declaration : Token.t list -> Ast.declaration partial = function
  | ({kind = Token_kind.Var; _} as var) :: ts -> (
      let* exp, ts = primary ts in
      match exp with
      | Ast.Literal (Ast.Identifier _ as identifier) -> (
        match ts with
        | ({kind = Token_kind.Equal; _} as equal) :: ts -> (
            let* expr, ts = expression ts in
            match (expr, ts) with
            | expr, ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
                Ok (Ast.Variable_declaration {var; identifier; assign = Some {equal; expr}; semicolon}, ts)
            | _ -> Error "After '=' expected to get an expression followed by a semicolon" )
        | ({kind = Token_kind.Semicolon; _} as semicolon) :: ts ->
            Ok (Ast.Variable_declaration {var; identifier; assign = None; semicolon}, ts)
        | _ -> Error "';' or '=' is expected to declare a variable." )
      | _ -> Error "A literal identifier after 'var' is expected to declare a variable name" )
  | _ -> Error "A variable declaration must start with 'var' keyword"

let rec block (ts : Token.t list) : Ast.statement partial =
  let rec aux acc = function
    | {kind = Token_kind.Right_brace; _} :: ts -> Ok (Ast.Block {statements = List.rev acc}, ts)
    | [{kind = Token_kind.Eof; _}] | [] -> Error "Expected closing brace '}'."
    | ts ->
        let* d, ts = declaration ts in
        aux (d :: acc) ts
  in
  aux [] ts

and statement (ts : Token.t list) : Ast.statement partial =
  match ts with
  | {kind = Token_kind.If; _} :: {kind = Token_kind.Left_paren; _} :: ts -> if_statement ts
  | {kind = Token_kind.For; _} :: _ ->
      (* For loop parsed and de-sugared as mostly a while loop *)
      for_statement ts
  | {kind = Token_kind.Left_brace; _} :: ts -> block ts
  | {kind = Token_kind.Print; _} :: ts ->
      let+ expr, ts = expression_statement ts in
      (Ast.Print_statement {expr}, ts)
  | {kind = Token_kind.While; _} :: {kind = Token_kind.Left_paren; _} :: ts -> while_statement ts
  | ts ->
      let+ expr, ts = expression_statement ts in
      (Ast.Expression_statement {expr}, ts)

and if_statement (ts : Token.t list) : Ast.statement partial =
  let* condition, ts = expression ts in
  match ts with
  | {kind = Token_kind.Right_paren; _} :: ts -> (
      let* if_body, ts = statement ts in
      match ts with
      | {kind = Token_kind.Else; _} :: ts ->
          let+ else_body, ts = statement ts in
          (Ast.If_statement {condition; if_body; else_body = Some else_body}, ts)
      | ts -> Ok (Ast.If_statement {condition; if_body; else_body = None}, ts) )
  | _ -> Error "Expected ')' after if condition."

(* A for loop does not have an AST representation because it is syntax sugar for a while loop. *)
and for_statement : Token.t list -> Ast.statement partial = function
  | {kind = Token_kind.For; _} :: {kind = Token_kind.Left_paren; _} :: ts ->
      let* for_initializer, ts =
        match variable_declaration ts with
        | Ok (var_decl, ts) -> Ok (Either.First var_decl, ts)
        | Error _ ->
            let+ expr_stmt, ts = expression_statement ts in
            (Either.Second expr_stmt, ts)
      in
      let* for_condition, ts = try_parse expression Token_kind.Semicolon ts in
      let* for_increment, ts = try_parse expression Token_kind.Right_paren ts in
      let* for_body, ts = statement ts in
      (* `for (initializer ; condition ; increment ) for_body`
       * is equivalent to:
       * `{ initializer; while (condition) { for_body ; increment } }` *)
      let condition = Option.value ~default:(Ast.Literal (Ast.Bool true)) for_condition in
      let for_body = Ast.Statement for_body in
      let for_increment =
        for_increment
        |> Option.map ~f:List.return
        |> Option.value ~default:[]
        |> List.map ~f:(fun expr -> Ast.Expression_statement {expr})
        |> List.map ~f:(fun stmt -> Ast.Statement stmt)
      in
      let body = Ast.Block {statements = for_body :: for_increment} in
      let for_as_while = Ast.While_statement {condition; body} in
      let for_initializer =
        Either.value_map for_initializer ~first:Fn.id ~second:(fun expr ->
            Ast.Statement (Ast.Expression_statement {expr}))
      in
      Ok (Ast.Block {statements = [for_initializer; Ast.Statement for_as_while]}, ts)
  | _ -> Error "Cannot parse a for loop. Missing for token."

and while_statement (ts : Token.t list) : Ast.statement partial =
  let* condition, ts = expression ts in
  match ts with
  | {kind = Token_kind.Right_paren; _} :: ts ->
      let+ body, ts = statement ts in
      (Ast.While_statement {condition; body}, ts)
  | _ -> Error "Expected ')' after if condition."

and declaration : Token.t list -> Ast.declaration partial = function
  | {kind = Token_kind.Var; _} :: _ as ts -> variable_declaration ts
  | ts ->
      let+ s, ts = statement ts in
      (Ast.Statement s, ts)

let rec loop (acc : Ast.declaration list) (ts : Token.t list) : (Ast.declaration list, string) Result.t =
  match ts with
  | [] | [{kind = Token_kind.Eof; _}] -> Ok acc
  | ts ->
      let* x, ts = declaration ts in
      loop (x :: acc) ts

let parse (ts : Token.t list) : (Ast.Program.t, string) Result.t =
  let+ x = loop [] ts in
  List.rev x |> Ast.Program.return
