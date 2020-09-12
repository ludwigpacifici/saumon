(** An environment represents bindings between a variable and its value. It's scope aware (delimited with "\{ ... \}"). *)
type t

val empty : unit -> t
(** Create an empty environment. *)

val is_empty : t -> bool
(** Check if the environment is empty, i.e. no variable declared. *)

val define : env:t -> id:string -> Value.t -> t
(** Define a variable. Shadowing is allowed. It follows what Scheme does. *)

val assign : env:t -> id:string -> Value.t -> t option
(** Assign a variable. Shadowing is allowed. If the variable is undeclared, no new Environment is returned. *)

val contains : env:t -> id:string -> bool
(** Returns true if a key is defined in the environment. *)

val get : env:t -> id:string -> Value.t option
(** Lookup the value of an identifier. *)

val push_scope : env:t -> t
(** Add a new scope. *)

val pop_scope : env:t -> (t * t) option
(** Return the dropped (most inner) scope and the rest of the environment. *)
