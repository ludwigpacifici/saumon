type t =
  { kind : Token_kind.t
  ; line : int
  ; column : int }
[@@deriving show, make, eq]

let of_token_kind ~kind =
  {kind; line = 1 (* Line starts at 1*); column = 0 (* Column starts at 1*)}
