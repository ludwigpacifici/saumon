open Omnipresent

type error =
  { location : Location.t
  ; where : string
  ; message : string }
[@@deriving show, eq]

open Base

let evaluate_binary_number (l : float) (t : Token.t) (r : float) :
    (Value.t, error) Result.t =
  let open Poly in
  match t.kind with
  | Token_kind.Plus -> Ok (Value.Number (l +. r))
  | Token_kind.Minus -> Ok (Value.Number (l -. r))
  | Token_kind.Star -> Ok (Value.Number (l *. r))
  | Token_kind.Slash -> Ok (Value.Number (l /. r))
  | Token_kind.Greater -> Ok (Value.Bool (l > r))
  | Token_kind.Greater_equal -> Ok (Value.Bool (l >= r))
  | Token_kind.Less -> Ok (Value.Bool (l < r))
  | Token_kind.Less_equal -> Ok (Value.Bool (l <= r))
  | t_kind ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Unknown binary operator to be used with two floats" }

let evaluate_binary_string (l : string) (t : Token.t) (r : string) :
    (Value.t, error) Result.t =
  match t.kind with
  | Token_kind.Plus -> Ok (Value.String (l ^ r))
  | t_kind ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Unknown binary operator to be used with two strings" }

let rec evaluate (env : Environment.t) (e : Ast.expression) :
    (Value.t, error) Result.t =
  match e with
  | Ast.Literal l -> evaluate_literal env l
  | Ast.Grouping (_, e, _) -> evaluate env e
  | Ast.Unary (t, e) -> evaluate_unary env t e
  | Ast.Binary (l, t, r) -> evaluate_binary env l t r

and evaluate_literal (env : Environment.t) (l : Ast.literal) :
    (Value.t, error) Result.t =
  match l with
  | Ast.Identifier id -> (
    (* Value.of_ast_literal does not resolve literal identifier *)
    match Environment.get ~env ~id with
    | Some v -> Ok v
    | None ->
        Error
          { location = Location.make ~line:0 ~column:0
          ; where = id
          ; message = "Undefined variable: " ^ id } )
  | l -> Ok (Value.of_ast_literal l)

and evaluate_unary (env : Environment.t) (t : Token.t) (e : Ast.expression) :
    (Value.t, error) Result.t =
  match (t.kind, evaluate env e) with
  | Token_kind.Minus, Ok (Value.Number e) -> Ok (Value.Number (-.e))
  | Token_kind.Minus, Ok _ ->
      Error
        { location = t.location
        ; where = "-"
        ; message = "Expected a float after a unary minus" }
  (* Lox follows Ruby’s simple rule: false and nil are falsey and everything
     else is truthy. *)
  | Token_kind.Bang, Ok (Value.Bool false) -> Ok (Value.Bool true)
  | Token_kind.Bang, Ok Value.Nil -> Ok (Value.Bool true)
  | Token_kind.Bang, _ -> Ok (Value.Bool false)
  | t_kind, _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Unknown unary operator" }

and evaluate_binary
    (env : Environment.t)
    (l : Ast.expression)
    (t : Token.t)
    (r : Ast.expression) : (Value.t, error) Result.t =
  match (evaluate env l, t.kind, evaluate env r) with
  | Ok l, Token_kind.Equal_equal, Ok r -> Ok (Value.Bool (Value.equal l r))
  | Ok l, Token_kind.Bang_equal, Ok r ->
      Ok (Value.Bool (Value.equal l r |> not))
  | Ok (Value.Number l), _, Ok (Value.Number r) -> evaluate_binary_number l t r
  | Ok (Value.String l), _, Ok (Value.String r) -> evaluate_binary_string l t r
  | Ok _, t_kind, Ok _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message =
            "Unknown binary operator when left and right expressions have \
             different types" }
  | Error _, t_kind, Ok _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Not able to evaluate the left expression" }
  | Ok _, t_kind, Error _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Not able to evaluate the right expression" }
  | Error _, t_kind, Error _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t_kind
        ; message = "Not able to evaluate both left and right expression" }

let execute_statement (env : Environment.t) (s : Ast.statement) :
    (Value.t option, error) Result.t =
  match s with
  | Ast.Expression_statement (e, _) ->
      evaluate env e
      |> Result.bind ~f:(fun _ ->
             (* Evaluate the expression in case of errors, but discard on
                purpose the computed value because there is no print statement
                for it *)
             Ok None)
  | Ast.Print_statement (_, e, _) ->
      evaluate env e |> Result.bind ~f:(fun v -> Ok (Some v))

let execute_variable_declaration
    (env : Environment.t)
    (v : Ast.variable_declaration) : (Environment.t, error) Result.t =
  match v with
  | _, Ast.Identifier id, None, _ ->
      (* A variable not explicitly initialized is implicitely set to "nil" *)
      Environment.define ~env ~id Value.Nil |> Result.return
  | _, Ast.Identifier id, Some (_, e), _ ->
      evaluate env e |> Result.map ~f:(Environment.define ~env ~id)
  | t, _, _, _ ->
      Error
        { location = t.location
        ; where = Token_kind.to_string t.kind
        ; message = "Cannot execute a variable declaration." }

let extract_values
    ((env : Environment.t), (vs : Value.t list))
    (d : Ast.declaration) =
  match d with
  | Ast.Statement s ->
      execute_statement env s
      |> Result.bind
           ~f:
             ( Option.map ~f:(fun v -> v :: vs)
             >> Option.value ~default:vs >> Result.return
             >> Result.map ~f:(fun vs -> (env, vs)) )
  | Ast.Variable_declaration v ->
      execute_variable_declaration env v
      |> Result.map ~f:(fun env -> (env, vs))

let stdout_print ((_ : Environment.t), (vs : Value.t list)) : unit =
  List.iter vs ~f:(Value.to_string >> Core.Out_channel.print_endline)

let execute_k
    ~(k : Environment.t * Value.t list -> unit)
    (env : Environment.t)
    (program : Ast.Program.t) : (unit, error) Result.t =
  List.fold_result ~init:(env, []) ~f:extract_values (Ast.Program.get program)
  |> Result.map ~f:k

let execute (env : Environment.t) (program : Ast.Program.t) :
    (unit, error) Result.t =
  execute_k ~k:stdout_print env program
