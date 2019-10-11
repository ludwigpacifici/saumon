(** An environment represents bindings between a variable and its value. It's
    scope aware (delimited with `{...}`). *)
type t

(** Create an empty environment. *)
val empty : unit -> t

(** Check if the environment is empty, i.e. no variable declared. *)
val is_empty : t -> bool

(** Define a variable. Shadowing is allowed. It follows what Scheme does. *)
val define : env:t -> id:string -> Value.t -> t

(** Assign a variable. Shadowing is allowed. If the variable is undeclared, no
    new Environment is returned. *)
val assign : env:t -> id:string -> Value.t -> t option

(** Returns true if a key is defined in the environment. *)
val contains : env:t -> id:string -> bool

(** Lookup the value of an identifier. *)
val get : env:t -> id:string -> Value.t option

(** Add a new scope. *)
val push_scope : env:t -> t

(** Return the dropped (most inner) scope and the rest of the environment. *)
val pop_scope : env:t -> (t * t) option
