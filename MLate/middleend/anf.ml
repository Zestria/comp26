[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Ast = MLate_frontend.Ast
module Names = Set.Make (String)
open Anf_ast

let ( let* ) = Result.bind

let initial_state = [], 0, []

(** Collect existing names, including free variables, to avoid capturing them. *)
let rec names used = function
  | Ast.Int _ -> used
  | Ast.Var name -> Names.add name used
  | Ast.Fun (name, body) -> names (Names.add name used) body
  | Ast.App (left, right) | Ast.Binop (_, left, right) ->
    names (names used left) right
  | Ast.If (cond, yes, no) ->
    names (names (names used cond) yes) no 
  | Ast.Let (binding, body) ->
    names (names (Names.add binding.name used) binding.value) body
;;

(** Simplify [let x = value in x] without removing or duplicating [value]. *)
let bind flag name value = function
  | ACExpr (CImm (IVar result)) when flag = Ast.Nonrec && name = result -> ACExpr value
  | body -> ALet (flag, name, value, body)
;;

(** Only function definitions are supported on the right of [let rec]. *)
let check_binding (binding : Ast.binding) =
  match binding.value with
  | Ast.Fun _ -> Ok ()
  | _ when binding.flag = Ast.Nonrec -> Ok ()
  | _ -> Error ("let rec requires a function: " ^ binding.name)
;;

let run program =
  let source_names =
    List.fold_left
      (fun used (binding : Ast.binding) ->
         names (Names.add binding.name used) binding.value)
      Names.empty
      program
  in
  let rec fresh_name next_id =
    let name = Printf.sprintf "anf%d" next_id in
    if Names.mem name source_names then fresh_name (next_id + 1) else name, next_id + 1
  in
  (* [env] maps source names to fresh names; [next_id] supplies unique IDs.
     [continue] receives a computation and builds the rest of the ANF expression.
     Any lets extracted from [expression] wrap that rest. *)
  let rec normalize env next_id expression k =
    match expression with
    | Ast.Int n -> k (CImm (IInt n)) next_id
    | Ast.Var name ->
      let found_name = Option.value (List.assoc_opt name env) ~default:name in
      k (CImm (IVar found_name)) next_id
    | Ast.Fun (parameter, body) ->
      let name, next_id = fresh_name next_id in
      let* body, next_id = normalize_term ((parameter, name) :: env) next_id body in
      k (CFun (name, body)) next_id
    | Ast.App (fn, arg) ->
      normalize_atom env next_id fn (fun fn next_id ->
        normalize_atom env next_id arg (fun argument next_id ->
          k (CApp (fn, argument)) next_id))
    | Ast.Binop (op, left, right) ->
      normalize_atom env next_id left (fun left next_id ->
        normalize_atom env next_id right (fun right next_id ->
          k (CBinop (op, left, right)) next_id))
    | Ast.If (condition, yes, no) ->
      normalize_atom env next_id condition (fun condition next_id ->
        let* yes, next_id = normalize_term env next_id yes in
        let* no, next_id = normalize_term env next_id no in
        k (CIf (condition, yes, no)) next_id)
    | Ast.Let (binding, body) ->
      let* _ = check_binding binding in
      let name, next_id = fresh_name next_id in
      let body_scope = (binding.name, name) :: env in
      (* A recursive name is also in scope in its own definition. *)
      let value_scope = if binding.flag = Ast.Rec then body_scope else env in
      normalize value_scope next_id binding.value (fun value next_id ->
        let* body, next_id = normalize body_scope next_id body k in
        Ok (bind binding.flag name value body, next_id))
  (* Operands must be atoms, so name a computation when necessary. *)
  and normalize_atom env next_id expression k =
    normalize env next_id expression (fun value next_id ->
      match value with
      | CImm atom -> k atom next_id
      | _ ->
        let name, next_id = fresh_name next_id in
        let* body, next_id = k (IVar name) next_id in
        Ok (bind Ast.Nonrec name value body, next_id))
  (* A complete body returns its final computation directly. *)
  and normalize_term env next_id expression =
    normalize env next_id expression (fun value next_id ->
      Ok (ACExpr value, next_id))
  in
  let normalize_toplevel state (binding : Ast.binding) =
    let* env, next_id, bindings = state in
    let* () = check_binding binding in
    let flag = binding.flag in
    let name, next_id = fresh_name next_id in
    let body_scope = (binding.name, name) :: env in
    let value_scope = if flag = Ast.Rec then body_scope else env in
    let* body, next_id = normalize_term value_scope next_id binding.value in
    Ok (body_scope, next_id, { flag; name; body } :: bindings)
  in
  let* _, _, bindings = List.fold_left normalize_toplevel (Ok initial_state) program in
  Ok (List.rev bindings)
;;
