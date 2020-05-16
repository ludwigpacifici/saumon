open Base
open Omnipresent

let ( let* ) x f = Result.bind ~f x

let ( let+ ) x f = Result.map ~f x

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

let rec evaluate (env : Environment.t) (expr : Ast.expression) :
    (Environment.t * Value.t, error) Result.t =
  match expr with
  | Ast.Literal l -> evaluate_literal env l
  | Ast.Grouping {expr; _} -> evaluate env expr
  | Ast.Unary {operator; expr} -> evaluate_unary env operator expr
  | Ast.Binary {left_expr; operator; right_expr} ->
      evaluate_binary env left_expr operator right_expr
  | Ast.Assignment {identifier = Ast.Identifier id; equal; expr} ->
      if Environment.contains ~env ~id then
        let* env, value = evaluate env expr in
        match Environment.assign ~env ~id value with
        | Some env -> Ok (env, value)
        | None ->
            Error
              { location = equal.location
              ; message =
                  "Unable to do assignement for "
                  ^ id
                  ^ " even though it is already defined." }
      else
        Error
          { location = equal.location
          ; message = "Undefined variable '" ^ id ^ "'" }
  | Ast.Assignment {equal; _} ->
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
  | Token_kind.Bang, Ok (env, v) ->
      let b = not (Value.is_truthy v) in
      Ok (env, Value.Bool b)
  | t_kind, _ ->
      Error
        { location = t.location
        ; message =
            "Unknown unary (" ^ Token_kind.to_string t_kind ^ ") operator" }

(* Return the left hand side if truthy, otherwise, return the right hand side *)
and evaluate_or (env : Environment.t) (l : Ast.expression) (r : Ast.expression)
    : (Environment.t * Value.t, error) Result.t =
  let* env, l = evaluate env l in
  if Value.is_truthy l then Ok (env, l) else evaluate env r

(* Return the left hand side if falsey, otherwise, return the right hand side *)
and evaluate_and (env : Environment.t) (l : Ast.expression) (r : Ast.expression)
    : (Environment.t * Value.t, error) Result.t =
  let* env, l = evaluate env l in
  if not (Value.is_truthy l) then Ok (env, l) else evaluate env r

and evaluate_binary
    (env : Environment.t)
    (l : Ast.expression)
    (t : Token.t)
    (r : Ast.expression) : (Environment.t * Value.t, error) Result.t =
  let greedy_evaluate (l : Ast.expression) (t : Token.t) (r : Ast.expression) =
    match (evaluate env l, t.kind, evaluate env r) with
    | Ok (_, l), Token_kind.Equal_equal, Ok (_, r) ->
        Ok (env, Value.Bool (Value.equal l r))
    | Ok (_, l), Token_kind.Bang_equal, Ok (_, r) ->
        Ok (env, Value.Bool (Value.equal l r |> not))
    | Ok (_, Value.Number l), _, Ok (_, Value.Number r) ->
        let+ v = evaluate_binary_number l t r in
        (env, v)
    | Ok (_, Value.String l), _, Ok (_, Value.String r) ->
        let+ v = evaluate_binary_string l t r in
        (env, v)
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
  in
  match t.kind with
  | Token_kind.Or -> evaluate_or env l r
  | Token_kind.And -> evaluate_and env l r
  | _ -> greedy_evaluate l t r

let rec execute_statement (env : Environment.t) (s : Ast.statement) :
    (Environment.t * Value.t list option, error) Result.t =
  let rec while_loop condition body (env, values) =
    let* env, condition_value = evaluate env condition in
    if Value.is_truthy condition_value then
      let* env, new_values = execute_statement env body in
      match (values, new_values) with
      | None, None -> while_loop condition body (env, None)
      | Some values, None -> while_loop condition body (env, Some values)
      | None, Some new_values -> while_loop condition body (env, Some new_values)
      | Some values, Some new_values ->
          while_loop condition body (env, Some (new_values @ values))
    else Ok (env, values)
  in
  match s with
  | Ast.Block {statements} -> execute_block env statements
  | Ast.Expression_statement {expr} ->
      let* env, _value = evaluate env expr in
      (* Evaluate the expression in case of errors, but discard on purpose the
         computed value because there is no print statement for it. Forward the
         environment because it can be updated by an assignment. *)
      Ok (env, None)
  | Ast.If_statement {condition; if_body; else_body} -> (
      let* env, condition = evaluate env condition in
      if Value.is_truthy condition then execute_statement env if_body
      else
        match else_body with
        | None -> Ok (env, None)
        | Some else_body -> execute_statement env else_body )
  | Ast.Print_statement {expr} ->
      let* env, v = evaluate env expr in
      Ok (env, Some [v])
  | Ast.While_statement {condition; body} ->
      while_loop condition body (env, None)

(* Run everything in the block and ensure the environment is
   pushed/updated/popped correctly. *)
and execute_block env ds =
  let env = Environment.push_scope ~env in
  let* env, vs =
    List.fold_result ds ~init:(env, []) ~f:(fun (env, acc) d ->
        let+ env, inner_vs = execute_declaration env d in
        let vs = Option.value inner_vs ~default:[] @ acc in
        (env, vs))
  in
  match Environment.pop_scope ~env with
  | Some (_dropped, env) -> Ok (env, Some vs)
  | None ->
      Error
        { location =
            (* FIXME: review error reporting with to provide line/column *)
            Location.start ()
        ; message = "Closing a block will remove the whole environment" }

and execute_variable_declaration
    (env : Environment.t)
    (v : Ast.variable_declaration) : (Environment.t, error) Result.t =
  match v with
  | {identifier = Ast.Identifier id; assign = None; _} ->
      (* A variable not explicitly initialized is implicitely set to "nil" *)
      Environment.define ~env ~id Value.Nil |> Result.return
  | {identifier = Ast.Identifier id; assign = Some {expr; _}; _} ->
      let+ env, value = evaluate env expr in
      Environment.define ~env ~id value
  | {var; _} ->
      Error
        { location = var.location
        ; message = "Cannot execute a variable declaration." }

and execute_declaration (env : Environment.t) (d : Ast.declaration) :
    (Environment.t * Value.t list option, error) Result.t =
  match d with
  | Ast.Statement s -> execute_statement env s
  | Ast.Variable_declaration v ->
      let+ env = execute_variable_declaration env v in
      (env, None)

let extract_values
    ((env : Environment.t), (vs : Value.t list))
    (d : Ast.declaration) =
  let* env, new_vs = execute_declaration env d in
  Option.map new_vs ~f:(fun new_vs -> new_vs @ vs)
  |> Option.value ~default:vs
  |> Result.return
  |> Result.map ~f:(fun vs -> (env, vs))

let stdout_print (_ : Environment.t) (vs : Value.t list) : unit =
  List.iter vs ~f:(Value.to_string >> Core.Out_channel.print_endline)

let execute_k
    ~(k : Environment.t -> Value.t list -> unit)
    (env : Environment.t)
    (program : Ast.Program.t) : (unit, error) Result.t =
  let+ env, vs =
    List.fold_result ~init:(env, []) ~f:extract_values (Ast.Program.get program)
  in
  k env (List.rev vs)

let execute (env : Environment.t) (program : Ast.Program.t) :
    (unit, error) Result.t =
  execute_k ~k:stdout_print env program
