type t

val empty : unit -> t
(** Create an empty environment. *)

val is_empty : t -> bool
(** Check if the environment is empty, i.e. no variable declared. *)

val define : env:t -> id:string -> Value.t -> t
(** Define a variable. Shadowing is allowed. It follows what Scheme does. *)

val get : env:t -> id:string -> Value.t option
(** Lookup the value of an identifier. *)
