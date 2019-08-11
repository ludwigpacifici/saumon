(* program ::= declaration * EOF
 *
 * declaration ::= variable_declaration
 *               | statement
 *
 * variable_declaration ::= "var" IDENTIFIER ( "=" expression )? ";" ;
 *
 * statement ::= expression_statement
 *             | print_statement
 *
 * expression_statement ::= expression ";"
 *
 * print_statement ::= "print" expression ";"
 *
 * expression ::= equality
 *
 * equality ::= comparison ( ( "!=" | "==" ) comparison )*
 *
 * comparison ::= addition ( ( ">" | ">=" | "<" | "<=" ) addition )*
 *
 * addition ::= multiplication ( ( "-" | "+" ) multiplication )*
 *
 * multiplication ::= unary ( ( "/" | "*" ) unary )*
 *
 * unary ::= ( "!" | "-" ) unary
 *         | primary
 *
 * primary ::= NUMBER
 *           | STRING
 *           | "false"
 *           | "true"
 *           | "nil"
 *           | "(" expression ")"
 *           | IDENTIFIER
 *)

type expression =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Grouping of grouping

and literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
  | Identifier of string

and unary = Token.t * expression

and binary = expression * Token.t * expression

and grouping = Token.t * expression * Token.t [@@deriving show]

type expression_statement =
  expression
  * (* The prefix token is constantly equal to token_kind.Semicolon , i.e. ";" *)
    Token.t
[@@deriving show]

type print_statement =
  (* The suffix token is constantly equal to token_kind.Prin , i.e. "print" *)
  Token.t
  * expression
  * (* The prefix token is constantly equal to token_kind.Semicolon , i.e. ";" *)
    Token.t
[@@deriving show]

type statement =
  | Expression_statement of expression_statement
  | Print_statement of print_statement
[@@deriving show]

type variable_declaration =
  (* Constantly equal to token_kind.Var, i.e. "var" *)
  Token.t
  * (* literal is of type identifier *)
    literal
  * (* Constantly equal to token_kind.Equal, i.e. "=" and an expression *)
  (Token.t * expression) option
  * (* Constantly equal to token_kind.Semicolon , i.e. ";" *)
    Token.t
[@@deriving show]

type declaration =
  | Variable_declaration of variable_declaration
  | Statement of statement
[@@deriving show]

type program = Program of declaration list [@@deriving show]

module Program = struct
  open Base

  let empty = Program []

  let return = function [] -> empty | x -> Program x

  let of_statement (x : statement) : program =
    Statement x |> List.return |> return

  let of_declaration (x : variable_declaration) : program =
    Variable_declaration x |> List.return |> return
end
