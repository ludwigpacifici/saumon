(* Lox Grammar : http://www.craftinginterpreters.com/appendix-i.html *)
type literal =
  | Bool of bool
  | Identifier of string
  | Nil
  | Number of float
  | String of string
[@@deriving show, eq]

type expression =
  | Assignment of {identifier: literal; equal: Token.t; expr: expression}
  | Binary of {left_expr: expression; operator: Token.t; right_expr: expression}
  | Grouping of {left_paren: Token.t; expr: expression; right_paren: Token.t}
  | Literal of literal
  | Unary of {operator: Token.t; expr: expression}
[@@deriving show, eq]

type variable_declaration =
  { var: Token.t
  ; identifier: literal
  ; assign: declaration_assign option
  ; semicolon: Token.t }

and declaration_assign = {equal: Token.t; expr: expression}
[@@deriving show, eq]

type declaration =
  | Statement of statement
  | Variable_declaration of variable_declaration
[@@deriving show, eq]

and statement =
  | Block of {statements: declaration list}
  | Expression_statement of {expr: expression}
  | If_statement of
      {condition: expression; if_body: statement; else_body: statement option}
  | Print_statement of {expr: expression}
  | While_statement of {condition: expression; body: statement}
[@@deriving show, eq]

module Program = struct
  type t = Program of declaration list [@@deriving show, eq]

  let empty = Program []
  let return : declaration list -> t = function [] -> empty | x -> Program x
  let of_statement (x : statement) : t = [Statement x] |> return
  let of_declaration (x : declaration) : t = [x] |> return
  let get (Program p) : declaration list = p
end
