type ('first, 'second) t =
  | First of 'first
  | Second of 'second
[@@deriving show, eq]
