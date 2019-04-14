type t =
  { kind : Token_kind.t
  ; line : int
  ; column : int }
[@@deriving show, make, eq]

let of_token_kind ~kind = {kind; line = 0; column = 0}
