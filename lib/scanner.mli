type error = {location: Location.t; message: string} [@@deriving show, eq]

val scan_tokens : string -> (Token.t list, error list) result
(** Note: the scanner drops comments. This is not ideal if one wants to write a Lox code formatter based on this
    library. *)
