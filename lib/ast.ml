(*
 * program ::= declaration * EOF ;
 *
 * declaration ::= variable_declaration
 *               | statement ;
 *
 * variable_declaration ::= "var" IDENTIFIER ( "=" expression )? ";" ;
 *
 * statement ::= expression_statement
 *             | print_statement
 *             | block ;
 *
 * block ::= "{" declaration* "}" ;
 *
 * expression_statement ::= expression ";" ;
 *
 * print_statement ::= "print" expression ";" ;
 *
 * expression ::= assignment o
 *
 * assignment ::= IDENTIFIER "=" assignment ;
 *               | equality ;
 *
 * equality ::= comparison ( ( "!=" | "==" ) comparison )* ;
 *
 * comparison ::= addition ( ( ">" | ">=" | "<" | "<=" ) addition )* ;
 *
 * addition ::= multiplication ( ( "-" | "+" ) multiplication )* ;
 *
 * multiplication ::= unary ( ( "/" | "*" ) unary )* ;
 *
 * unary ::= ( "!" | "-" ) unary
 *         | primary ;
 *
 * primary ::= NUMBER
 *           | STRING
 *           | "false"
 *           | "true"
 *           | "nil"
 *           | "(" expression ")"
 *           | IDENTIFIER ;
 *)

type expression =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Grouping of grouping
  | Assignment of assignment

and literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil
  | Identifier of string

and unary = Token.t * expression

and binary = expression * Token.t * expression

and grouping = Token.t * expression * Token.t

and assignment =
  literal (* Constantly equal to token_kind.Identifier *)
  * Token.t (* Constantly equal to Identifier *)
  * expression
[@@deriving show, eq]

type expression_statement =
  expression
  * (* The prefix token is constantly equal to token_kind.Semicolon, i.e. ";" *)
    Token.t
[@@deriving show, eq]

type print_statement =
  Token.t
  (* The suffix token is constantly equal to token_kind.Print, i.e. "print" *)
  * expression
  * (* The prefix token is constantly equal to token_kind.Semicolon, i.e. ";" *)
    Token.t
[@@deriving show, eq]

type variable_declaration =
  Token.t (* Constantly equal to token_kind.Var, i.e. "var" *)
  * literal (* literal is of type identifier *)
  * ( Token.t
      (* Constantly equal to token_kind.Equal, i.e. "=" and an expression *)
    * expression )
    option
  * (* Constantly equal to token_kind.Semicolon , i.e. ";" *)
    Token.t
[@@deriving show, eq]

type declaration =
  | Variable_declaration of variable_declaration
  | Statement of statement

and statement =
  | Expression_statement of expression_statement
  | Print_statement of print_statement
  | Block of block

and block =
  (* Constantly equal to token_kind.Left_brace. *)
  Token.t
  * declaration list
  * (* Constantly equal to token_kind.Right_brace. *)
    Token.t
[@@deriving show, eq]

module Program = struct
  type t = Program of declaration list [@@deriving show, eq]

  let empty = Program []

  let return : declaration list -> t = function [] -> empty | x -> Program x

  let of_statement (x : statement) : t = [Statement x] |> return

  let of_declaration (x : variable_declaration) : t =
    [Variable_declaration x] |> return

  let get (Program p) : declaration list = p
end
