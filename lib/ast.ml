type program = Program of statement list

and statement =
  | Expression_statement of expression_statement
  | Print_statement of print_statement
  | NoOperation

and expression_statement =
  expression
  * (* The prefix token is constantly equal to token_kind.Semicolon , i.e. ";" *)
    Token.t

and print_statement =
  (* The suffix token is constantly equal to token_kind.Prin , i.e. "print" *)
  Token.t
  * expression
  * (* The prefix token is constantly equal to token_kind.Semicolon , i.e. ";" *)
    Token.t

and expression =
  | Literal of literal
  | Unary of unary
  | Binary of binary
  | Grouping of grouping

and literal =
  | Number of float
  | String of string
  | Bool of bool
  | Nil

and unary = Token.t * expression

and binary = expression * Token.t * expression

and grouping = Token.t * expression * Token.t [@@deriving show, eq]

let make_program x = Program x
