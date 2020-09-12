type t = {line: int; column: int} [@@deriving show, make, eq]

let start () = {line= 1; column= 0}
let next_column x c = {x with column= x.column + c}
let next_line x = {line= x.line + 1; column= 0}

let show_compact x =
  let open Base in
  Int.to_string x.line ^ ":" ^ Int.to_string x.column
