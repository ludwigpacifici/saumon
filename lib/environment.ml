open Base

type t = (string, Value.t, String.comparator_witness) Map.t

let empty () = Map.empty (module String)

let is_empty (env : t) : bool = Map.is_empty env

let define ~(env : t) ~(id : string) (value : Value.t) =
  Map.update env id ~f:(function None -> value | Some _ -> value)

let get ~(env : t) ~(id : string) = Map.find env id
