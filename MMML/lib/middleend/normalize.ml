[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module Env = Map.Make (String)

type error =
  | Recursive_wildcard
  | Recursive_non_function of string

exception Unsupported of error

let pp_error fmt = function
  | Recursive_wildcard -> Format.pp_print_string fmt "recursive wildcard is unsupported"
  | Recursive_non_function name ->
    Format.fprintf fmt "recursive binding %S must have a lambda RHS" name
;;

let initial bindings =
  List.fold_left (fun env (name, value) -> Env.add name value env) Env.empty bindings
;;

let lookup env name =
  match Env.find_opt name env with
  | Some value -> value
  | None -> External name
;;

let bind_pattern env pattern value =
  match pattern with
  | Ast.PWildcard -> env
  | Ast.PVariable name -> Env.add name value env
;;

let hint = function
  | Ast.PWildcard -> "_"
  | Ast.PVariable name -> name
;;

(* The local stamp supply is shared by CPS continuations so every generated binder gets a unique identifier within this normalization run. *)
[@@@zanuda "-mutability_check"]

let worker () =
  let next = ref 0 in
  let fresh hint =
    let stamp = !next in
    incr next;
    { stamp; hint }
  in
  let rec go env expression k =
    match expression with
    | Ast.Constant constant -> k (Value (Constant constant))
    | Ast.Variable name -> k (Value (lookup env name))
    | Ast.Lambda (parameter, body) ->
      let parameter_id = fresh (hint parameter) in
      let body = tail (bind_pattern env parameter (Local parameter_id)) body in
      k (Lambda (parameter_id, body))
    | Ast.Application (Ast.Application (Ast.Variable name, left), right) ->
      (match lookup env name with
       | Builtin (Binary op) ->
         atom env left (fun left ->
           atom env right (fun right -> k (Binary_primitive (op, left, right))))
       | Builtin And -> logical env left right false k
       | Builtin Or -> logical env left right true k
       | Constant _ | Local _ | External _ | Builtin (Unary _) ->
         application env (Ast.Application (Ast.Variable name, left)) right k)
    | Ast.Application (Ast.Variable name, argument) ->
      (match lookup env name with
       | Builtin (Unary op) ->
         atom env argument (fun arg -> k (Unary_primitive (op, arg)))
       | Constant _ | Local _ | External _ | Builtin (Binary _ | And | Or) ->
         application env (Ast.Variable name) argument k)
    | Ast.Application (fn, argument) -> application env fn argument k
    | Ast.If_then_else (condition, yes, no) ->
      atom env condition (fun condition ->
        let yes = tail env yes in
        let no = tail env no in
        k (If (condition, yes, no)))
    | Ast.Let_in (Ast.Nonrecursive, pattern, rhs, body) ->
      go env rhs (function
        | Value ((Constant _ | Local _ | Builtin _) as value) ->
          go (bind_pattern env pattern value) body k
        | ( Value (External _)
          | Apply _ | Unary_primitive _ | Binary_primitive _ | Lambda _ | If _ ) as rhs ->
          let id = fresh (hint pattern) in
          let continuation = go (bind_pattern env pattern (Local id)) body k in
          Let (id, rhs, continuation))
    | Ast.Let_in (Ast.Recursive, pattern, rhs, continuation) ->
      let fn, parameter, body, env = recursive env pattern rhs in
      let continuation = go env continuation k in
      Let_rec (fn, parameter, body, continuation)
  and atom env expression k =
    go env expression (function
      | Value ((Constant _ | Local _ | Builtin _) as value) -> k value
      | rhs ->
        (* External lookup is evaluated here too: errors in the function
           expression must precede effects in its argument. *)
        let id = fresh "anf" in
        let continuation = k (Local id) in
        Let (id, rhs, continuation))
  and tail env expression = go env expression (fun rhs -> Result rhs)
  and application env fn argument k =
    atom env fn (fun fn -> atom env argument (fun argument -> k (Apply (fn, argument))))
  and logical env left right short_value k =
    atom env left (fun left ->
      let right = tail env right in
      let short = Result (Value (Constant (Ast.Boolean short_value))) in
      let yes, no = if short_value then short, right else right, short in
      k (If (left, yes, no)))
  and recursive env pattern rhs =
    match pattern, rhs with
    | Ast.PWildcard, _ -> raise (Unsupported Recursive_wildcard)
    | Ast.PVariable name, Ast.Lambda (parameter, body) ->
      let fn = fresh name in
      let env = Env.add name (Local fn) env in
      let parameter_id = fresh (hint parameter) in
      let body = tail (bind_pattern env parameter (Local parameter_id)) body in
      fn, parameter_id, body, env
    | Ast.PVariable name, _ -> raise (Unsupported (Recursive_non_function name))
  in
  let rec returned_builtin = function
    | Result (Value (Builtin _ as builtin)) -> Some builtin
    | Let (_, _, continuation) | Let_rec (_, _, _, continuation) ->
      returned_builtin continuation
    | Result
        ( Value (Constant _ | Local _ | External _)
        | Apply _ | Unary_primitive _ | Binary_primitive _ | Lambda _ | If _ ) -> None
  in
  let definition env (Ast.Definition (flag, pattern, rhs)) =
    match flag with
    | Ast.Recursive ->
      let fn, parameter, body, env = recursive env pattern rhs in
      Recursive_definition (fn, parameter, body), env
    | Ast.Nonrecursive ->
      let id = fresh (hint pattern) in
      let rhs = tail env rhs in
      let binding =
        match returned_builtin rhs with
        | Some builtin -> builtin
        | None -> Local id
      in
      Definition (id, rhs), bind_pattern env pattern binding
  in
  tail, definition
;;

[@@@zanuda "+mutability_check"]

let expression ?(bindings = default_bindings) input =
  let tail, _ = worker () in
  try Ok (tail (initial bindings) input) with
  | Unsupported error -> Error error
;;

let program ?(bindings = default_bindings) input =
  let _, definition = worker () in
  let rec loop env reversed = function
    | [] -> List.rev reversed
    | source :: rest ->
      let normalized, env = definition env source in
      loop env (normalized :: reversed) rest
  in
  try Ok (loop (initial bindings) [] input) with
  | Unsupported error -> Error error
;;
