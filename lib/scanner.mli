type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show, eq]

(** Note: the scanner drops comments. This is not ideal if one wants to write a
    Lox code formatter based on this library. *)
val scan_tokens : string -> (Token.t list, error list) result
