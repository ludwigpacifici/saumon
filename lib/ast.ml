(* Lox Grammar : http://www.craftinginterpreters.com/appendix-i.html *)

type expression =
  | Assignment of assignment
  | Binary of binary
  | Grouping of grouping
  | Literal of literal
  | Unary of unary

and literal =
  | Bool of bool
  | Identifier of string
  | Nil
  | Number of float
  | String of string

and unary = Token.t * expression

and binary = expression * Token.t * expression

and grouping = Token.t * expression * Token.t

(* TODO: remove all the Token.t that should be set to a constant value. They do
   not have added value, plus it makes rewritting the AST harder. *)
and assignment =
  literal (* Constantly equal to token_kind.Identifier *)
  * Token.t (* Constantly equal to Identifier *)
  * expression
[@@deriving show, eq]

type expression_statement = expression [@@deriving show, eq]

type print_statement = expression [@@deriving show, eq]

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
  | Statement of statement
  | Variable_declaration of variable_declaration

and statement =
  | Block of block
  | Expression_statement of expression_statement
  | If_statement of if_statement
  | Print_statement of print_statement
  | While_statement of while_statement

and block = declaration list

and if_statement =
  Token.t (* Constantly equal to token_kind.If *)
  * Token.t (* Constantly equal to token_kind.Left_paren *)
  * expression
  * Token.t (* Constantly equal to token_kind.Right_paren *)
  * statement
  * (Token.t (* Constantly equal to token_kind.Else *) * statement) option

and while_statement =
  expression
  (* If evaluated to true then run the while body otherwise stop the while loop *)
  * statement
(* Body of the while loop *)
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
