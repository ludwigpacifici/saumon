type t

val make : kind:Token_kind.t -> line:int -> column:int -> t

val show : t -> string
