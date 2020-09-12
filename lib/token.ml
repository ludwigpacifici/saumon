type t = {kind: Token_kind.t; location: Location.t} [@@deriving show, make, eq]

let of_token_kind ~kind = {kind; location= Location.start ()}
