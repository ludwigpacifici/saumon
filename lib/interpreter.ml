open Omnipresent

type error =
  { location : Location.t
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
        ; message =
            "Unknown binary operator ("
            ^ Token_kind.to_string t_kind
            ^ ") to be used with two floats" }

let evaluate_binary_string (l : string) (t : Token.t) (r : string) :
    (Value.t, error) Result.t =
  match t.kind with
  | Token_kind.Plus -> Ok (Value.String (l ^ r))
  | t_kind ->
      Error
        { location = t.location
        ; message =
            "Unknown binary operator ("
            ^ Token_kind.to_string t_kind
            ^ ") to be used with two strings" }

let rec evaluate (env : Environment.t) (e : Ast.expression) :
    (Environment.t * Value.t, error) Result.t =
  match e with
  | Ast.Literal l -> evaluate_literal env l
  | Ast.Grouping (_, e, _) -> evaluate env e
  | Ast.Unary (t, e) -> evaluate_unary env t e
  | Ast.Binary (l, t, r) -> evaluate_binary env l t r
  | Ast.Assignment (Ast.Identifier id, equal, expression) ->
      if Environment.contains ~env ~id then
        evaluate env expression
        |> Result.bind ~f:(fun (env, value) ->
               match Environment.assign ~env ~id value with
               | Some env -> Ok (env, value)
               | None ->
                   Error
                     { location = equal.location
                     ; message =
                         "Unable to do assignement for "
                         ^ id
                         ^ " even though it is already defined." })
      else
        Error
          { location = equal.location
          ; message = "Undefined variable '" ^ id ^ "'" }
  | Ast.Assignment (_, equal, _) ->
      Error
        { location = equal.location
        ; message = "Expected to have an identifer on the left of '='" }

and evaluate_literal (env : Environment.t) (l : Ast.literal) :
    (Environment.t * Value.t, error) Result.t =
  match l with
  | Ast.Identifier id -> (
    (* Value.of_ast_literal does not resolve literal identifier *)
    match Environment.get ~env ~id with
    | Some v -> Ok (env, v)
    | None ->
        Error
          { location = Location.make ~line:0 ~column:0
          ; message = "Undefined variable: " ^ id } )
  | l -> Ok (env, Value.of_ast_literal l)

and evaluate_unary (env : Environment.t) (t : Token.t) (e : Ast.expression) :
    (Environment.t * Value.t, error) Result.t =
  match (t.kind, evaluate env e) with
  | Token_kind.Minus, Ok (env, Value.Number e) -> Ok (env, Value.Number (-.e))
  | Token_kind.Minus, Ok _ ->
      Error
        {location = t.location; message = "Expected a float after a unary minus"}
  (* Lox follows Ruby’s simple rule: false and nil are falsey and everything
     else is truthy. *)
  | Token_kind.Bang, Ok (env, Value.Bool false) -> Ok (env, Value.Bool true)
  | Token_kind.Bang, Ok (env, Value.Nil) -> Ok (env, Value.Bool true)
  | Token_kind.Bang, _ -> Ok (env, Value.Bool false)
  | t_kind, _ ->
      Error
        { location = t.location
        ; message =
            "Unknown unary (" ^ Token_kind.to_string t_kind ^ ") operator" }

and evaluate_binary
    (env : Environment.t)
    (l : Ast.expression)
    (t : Token.t)
    (r : Ast.expression) : (Environment.t * Value.t, error) Result.t =
  match (evaluate env l, t.kind, evaluate env r) with
  | Ok (_, l), Token_kind.Equal_equal, Ok (_, r) ->
      Ok (env, Value.Bool (Value.equal l r))
  | Ok (_, l), Token_kind.Bang_equal, Ok (_, r) ->
      Ok (env, Value.Bool (Value.equal l r |> not))
  | Ok (_, Value.Number l), _, Ok (_, Value.Number r) ->
      evaluate_binary_number l t r |> Result.map ~f:(fun v -> (env, v))
  | Ok (_, Value.String l), _, Ok (_, Value.String r) ->
      evaluate_binary_string l t r |> Result.map ~f:(fun v -> (env, v))
  | Ok _, t_kind, Ok _ ->
      Error
        { location = t.location
        ; message =
            "Unknown binary operator ("
            ^ Token_kind.to_string t_kind
            ^ ") when left and right expressions have different types" }
  | Error _, _, Ok _ ->
      Error
        { location = t.location
        ; message = "Not able to evaluate the left expression" }
  | Ok _, _, Error _ ->
      Error
        { location = t.location
        ; message = "Not able to evaluate the right expression" }
  | Error _, _, Error _ ->
      Error
        { location = t.location
        ; message = "Not able to evaluate both left and right expression" }

let rec execute_statement (env : Environment.t) (s : Ast.statement) :
    (Environment.t * Value.t list option, error) Result.t =
  match s with
  | Ast.Expression_statement (e, _) ->
      evaluate env e
      |> Result.bind ~f:(fun (env, _value) ->
             (* Evaluate the expression in case of errors, but discard on
                purpose the computed value because there is no print statement
                for it. Forward the environment because it can be updated by an
                assignment. *)
             Ok (env, None))
  | Ast.Print_statement (_, e, _) ->
      evaluate env e |> Result.bind ~f:(fun (env, v) -> Ok (env, Some [v]))
  | Ast.Block (_, ds, end_block) -> execute_block env ds end_block

(* Run everything in the block and ensure the environment is
   pushed/updated/popped correctly. *)
and execute_block env ds end_block =
  let env = Environment.push_scope ~env in
  List.fold_result ds ~init:(env, []) ~f:(fun (env, acc) d ->
      execute_declaration env d
      |> Result.map ~f:(fun (env, inner_vs) ->
             let vs = Option.value inner_vs ~default:[] @ acc in
             (env, vs)))
  |> Result.bind ~f:(fun (env, vs) ->
         match Environment.pop_scope ~env with
         | Some (_dropped, env) -> Ok (env, Some vs)
         | None ->
             Error
               { location = end_block.location
               ; message = "Closing a block will remove the whole environment"
               })

and execute_variable_declaration
    (env : Environment.t)
    (v : Ast.variable_declaration) : (Environment.t, error) Result.t =
  match v with
  | _, Ast.Identifier id, None, _ ->
      (* A variable not explicitly initialized is implicitely set to "nil" *)
      Environment.define ~env ~id Value.Nil |> Result.return
  | _, Ast.Identifier id, Some (_, e), _ ->
      evaluate env e
      |> Result.map ~f:(fun (env, value) -> Environment.define ~env ~id value)
  | t, _, _, _ ->
      Error
        { location = t.location
        ; message = "Cannot execute a variable declaration." }

and execute_declaration (env : Environment.t) (d : Ast.declaration) :
    (Environment.t * Value.t list option, error) Result.t =
  match d with
  | Ast.Statement s -> execute_statement env s
  | Ast.Variable_declaration v ->
      execute_variable_declaration env v
      |> Result.map ~f:(fun env -> (env, None))

let extract_values
    ((env : Environment.t), (vs : Value.t list))
    (d : Ast.declaration) =
  execute_declaration env d
  |> Result.bind ~f:(fun (env, new_vs) ->
         Option.map new_vs ~f:(fun new_vs -> new_vs @ vs)
         |> Option.value ~default:vs
         |> Result.return
         |> Result.map ~f:(fun vs -> (env, vs)))

let stdout_print (_ : Environment.t) (vs : Value.t list) : unit =
  List.iter vs ~f:(Value.to_string >> Core.Out_channel.print_endline)

let execute_k
    ~(k : Environment.t -> Value.t list -> unit)
    (env : Environment.t)
    (program : Ast.Program.t) : (unit, error) Result.t =
  List.fold_result ~init:(env, []) ~f:extract_values (Ast.Program.get program)
  |> Result.map ~f:(fun (env, vs) -> k env (List.rev vs))

let execute (env : Environment.t) (program : Ast.Program.t) :
    (unit, error) Result.t =
  execute_k ~k:stdout_print env program
