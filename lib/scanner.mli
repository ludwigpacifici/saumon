type t

val scan_tokens : string -> Token.t list * bool

val show : t -> string
