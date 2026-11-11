[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Ast = MLate_frontend.Ast
module Names = Set.Make (String)
open Anf_ast

(* Collect free names to detect unused bindings. *)
let free_atom = function
  | IInt _ -> Names.empty
  | IVar name -> Names.singleton name
;;

let rec free_computation = function
  | CImm atom -> free_atom atom
  | CBinop (_, left, right) | CApp (left, right) ->
    Names.union (free_atom left) (free_atom right)
  | CFun (parameter, body) -> Names.remove parameter (free_body body)
  | CIf (condition, yes, no) ->
    Names.union (free_atom condition) (Names.union (free_body yes) (free_body no))

and free_body = function
  | ACExpr value -> free_computation value
  | ALet (flag, name, value, body) ->
    let used = free_computation value in
    let used = if flag = Ast.Rec then Names.remove name used else used in
    Names.union used (Names.remove name (free_body body))
;;

(* All current integer operators terminate without effects or exceptions.
   A function body is deferred; a call may have effects or fail to terminate. *)
let rec safe_computation = function
  | CImm _ | CBinop _ | CFun _ -> true
  | CApp _ -> false
  | CIf (_, yes, no) -> safe_body yes && safe_body no

and safe_body = function
  | ACExpr value -> safe_computation value
  | ALet (_, _, value, body) -> safe_computation value && safe_body body
;;

let make_let flag name value body =
  if (not (Names.mem name (free_body body))) && safe_computation value
  then body
  else (
    match body with
    | ACExpr (CImm (IVar result)) when flag = Ast.Nonrec && name = result -> ACExpr value
    | _ -> ALet (flag, name, value, body))
;;

let substitute env = function
  | IVar name as atom -> Option.value (List.assoc_opt name env) ~default:atom
  | IInt _ as atom -> atom
;;

let fold_binop op left right =
  match left, right with
  | IInt left, IInt right ->
    let result =
      match op with
      | Ast.Plus -> left + right
      | Ast.Minus -> left - right
      | Ast.Times -> left * right
      | Ast.Lt -> if left < right then 1 else 0
    in
    CImm (IInt result)
  | _ -> CBinop (op, left, right)
;;

let rec simplify_body env = function
  | ACExpr value -> simplify_computation env value
  | ALet (flag, name, value, body) ->
    let value = simplify_computation env value in
    bind_result env flag name value body

and simplify_computation env = function
  | CImm atom -> ACExpr (CImm (substitute env atom))
  | CBinop (op, left, right) ->
    ACExpr (fold_binop op (substitute env left) (substitute env right))
  | CApp (fn, argument) -> ACExpr (CApp (substitute env fn, substitute env argument))
  | CFun (parameter, body) -> ACExpr (CFun (parameter, simplify_body env body))
  | CIf (condition, yes, no) ->
    (match substitute env condition with
     | IInt 0 -> simplify_body env no
     | IInt _ -> simplify_body env yes
     | condition -> ACExpr (CIf (condition, simplify_body env yes, simplify_body env no)))

(* A selected branch may contain lets. Move them before the original binding,
   then propagate an atomic result or keep its computation exactly once. *)
and bind_result env flag name value body =
  match value with
  | ALet (inner_flag, inner_name, inner_value, rest) ->
    make_let inner_flag inner_name inner_value (bind_result env flag name rest body)
  | ACExpr (CImm atom) when flag = Ast.Nonrec -> simplify_body ((name, atom) :: env) body
  | ACExpr value -> make_let flag name value (simplify_body env body)
;;

let run program =
  let simplify_binding (env, bindings) binding =
    let body = simplify_body env binding.body in
    let env =
      match binding.flag, body with
      | Ast.Nonrec, ACExpr (CImm atom) -> (binding.name, atom) :: env
      | _ -> env
    in
    env, { binding with body } :: bindings
  in
  let _, bindings = List.fold_left simplify_binding ([], []) program in
  List.rev bindings
;;
