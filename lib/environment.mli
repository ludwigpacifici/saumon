type t

(** Create an empty environment. *)
val empty : unit -> t

(** Check if the environment is empty, i.e. no variable declared. *)
val is_empty : t -> bool

(** Define a variable. Shadowing is allowed. It follows what Scheme does. *)
val define : env:t -> id:string -> Value.t -> t

(** Returns true if a key is defined in the environment. *)
val contains : env:t -> id:string -> bool

(** Lookup the value of an identifier. *)
val get : env:t -> id:string -> Value.t option
