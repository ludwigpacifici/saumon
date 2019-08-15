type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show, eq]

val scan_tokens : string -> (Token.t list, error list) result
