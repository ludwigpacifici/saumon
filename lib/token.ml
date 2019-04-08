type t =
  { kind : Token_kind.t
  ; line : int
  ; column : int }
[@@deriving show, make]
