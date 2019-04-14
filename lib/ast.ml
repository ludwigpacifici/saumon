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

and unary = Token.t * expression

and binary = expression * Token.t * expression

and grouping = Token.t * expression * Token.t [@@deriving show, eq]
