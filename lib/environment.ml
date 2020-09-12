open Base

type t = (string, Value.t, String.comparator_witness) Map.t list

let empty_scope () = Map.empty (module String)
let empty () = empty_scope () |> List.return

let is_empty (env : t) : bool =
  List.map ~f:Map.is_empty env |> List.fold ~init:true ~f:( && )

let rec define ~(env : t) ~(id : string) (value : Value.t) : t =
  match env with
  | e :: es ->
      Map.update e id ~f:(function None -> value | Some _ -> value) :: es
  | [] -> define ~env:(empty ()) ~id value

let assign ~(env : t) ~(id : string) (value : Value.t) : t option =
  let rec aux acc env =
    match env with
    | e :: es when Map.mem e id ->
        Map.update e id ~f:(function None -> value | Some _ -> value)
        |> Fn.flip List.cons acc |> List.rev |> Fn.flip List.append es
        |> Option.return
    | e :: es -> aux (e :: acc) es
    | [] -> (* Sad path: no declaration found in any scopes*) None in
  aux [] env

let contains ~(env : t) ~(id : string) : bool =
  List.exists env ~f:(fun e -> Map.mem e id)

let get ~(env : t) ~(id : string) : Value.t option =
  List.find_map ~f:(fun e -> Map.find e id) env

let push_scope ~(env : t) : t = empty_scope () :: env

let pop_scope ~(env : t) : (t * t) option =
  match env with [] | [_] -> None | e :: es -> Some (List.return e, es)
