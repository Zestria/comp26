[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf

let binder_counts program =
  let counts : (string, int) Hashtbl.t = Hashtbl.create 64 in
  let bump name =
    let current = Option.value (Hashtbl.find_opt counts name) ~default:0 in
    Hashtbl.replace counts name (current + 1)
  in
  let rec cexpr = function
    | CImm _ | CApp _ | CTuple _ | CMatchFailure -> ()
    | CFun (params, body) ->
      List.iter bump params;
      aexpr body
    | CIte (_, then_branch, else_branch) ->
      aexpr then_branch;
      aexpr else_branch
  and aexpr = function
    | ACExpr expr -> cexpr expr
    | ALet (name, value, body) ->
      cexpr value;
      bump name;
      aexpr body
    | ALetTuple (names, subject, body) ->
      List.iter (Option.iter bump) names;
      aexpr body
    | ALetRec (bindings, body) ->
      List.iter
        (fun (name, value) ->
           bump name;
           cexpr value)
        bindings;
      aexpr body
  in
  let item = function
    | AValue (name, body) ->
      bump name;
      aexpr body
    | ARec bindings ->
      List.iter
        (fun (name, value) ->
           bump name;
           cexpr value)
        bindings
    | AEval body -> aexpr body
  in
  List.iter item program;
  counts
;;

(* ---------- simplification ----------
   let x = e in x a_1...a_n  ==>  e a_1...a_n

   The other simplifications (let x = e in x, dead bindings) are already
   done in Anf_simplify.  The binding this targets is born in closure
   conversion (an application in value position), so it can only be
   caught here, after CC. *)

let occurs_imm name = function
  | ImmVar var -> String.equal name var
  | ImmInt _ | ImmBool _ | ImmUnit -> false
;;

let occurs_imms name imms = List.exists (occurs_imm name) imms

let rec simplify_aexpr (expr : aexpr) : aexpr =
  let expr =
    match expr with
    | ALet (name, value, body) -> ALet (name, simplify_cexpr value, simplify_aexpr body)
    | ALetTuple (names, subject, body) -> ALetTuple (names, subject, simplify_aexpr body)
    | ALetRec (bindings, body) ->
      ALetRec
        ( List.map (fun (name, value) -> name, simplify_cexpr value) bindings
        , simplify_aexpr body )
    | ACExpr cexpr -> ACExpr (simplify_cexpr cexpr)
  in
  simplify_top expr

and simplify_cexpr = function
  | CIte (cond, then_branch, else_branch) ->
    CIte (cond, simplify_aexpr then_branch, simplify_aexpr else_branch)
  | CFun (params, body) -> CFun (params, simplify_aexpr body)
  | (CImm _ | CApp _ | CTuple _ | CMatchFailure) as cexpr -> cexpr

and simplify_top (expr : aexpr) : aexpr =
  let reduced =
    match expr with
    | ALet (name, value, ACExpr (CApp (ImmVar callee, args)))
      when String.equal name callee && not (occurs_imms name args) ->
      (match value with
       | CImm imm -> Some (ACExpr (CApp (imm, args)))
       | CApp (fn, args') -> Some (ACExpr (CApp (fn, args' @ args)))
       | _ -> None)
    | _ -> None
  in
  match reduced with
  | None -> expr
  | Some expr -> simplify_top expr
;;

let simplify_item = function
  | AValue (name, body) -> AValue (name, simplify_aexpr body)
  | ARec bindings ->
    ARec (List.map (fun (name, value) -> name, simplify_cexpr value) bindings)
  | AEval body -> AEval (simplify_aexpr body)
;;

type ctx =
  { gen : Fresh.t
  ; counts : (string, int) Hashtbl.t
  }

let can_reuse ctx name = Option.value (Hashtbl.find_opt ctx.counts name) ~default:0 = 1

let rec hoist_aexpr ctx = function
  | ALet (name, CFun (params, body), cont) ->
    let body', body_items = hoist_aexpr ctx body in
    let cont', cont_items = hoist_aexpr ctx cont in
    let lifted, binding =
      if can_reuse ctx name
      then name, cont'
      else (
        let fresh = Fresh.fresh ~suffix:"__ll" ctx.gen name in
        fresh, ALet (name, CImm (ImmVar fresh), cont'))
    in
    binding, body_items @ [ AValue (lifted, ACExpr (CFun (params, body'))) ] @ cont_items
  | ALet (name, value, cont) ->
    let value', value_items = hoist_cexpr ctx value in
    let cont', cont_items = hoist_aexpr ctx cont in
    ALet (name, value', cont'), value_items @ cont_items
  | ACExpr (CFun (params, body)) ->
    let body', body_items = hoist_aexpr ctx body in
    let fresh = Fresh.fresh ~suffix:"__ll" ctx.gen "f" in
    ( ACExpr (CImm (ImmVar fresh))
    , body_items @ [ AValue (fresh, ACExpr (CFun (params, body'))) ] )
  | ACExpr value ->
    let value', value_items = hoist_cexpr ctx value in
    ACExpr value', value_items
  | ALetTuple (names, subject, cont) ->
    let cont', cont_items = hoist_aexpr ctx cont in
    ALetTuple (names, subject, cont'), cont_items
  | ALetRec (bindings, cont) ->
    let bindings', body_items = hoist_bindings ctx bindings in
    let cont', cont_items = hoist_aexpr ctx cont in
    ALetRec (bindings', cont'), body_items @ cont_items

and hoist_cexpr ctx = function
  | (CImm _ | CApp _ | CTuple _ | CMatchFailure) as value -> value, []
  | CIte (cond, then_branch, else_branch) ->
    let then_branch', then_items = hoist_aexpr ctx then_branch in
    let else_branch', else_items = hoist_aexpr ctx else_branch in
    CIte (cond, then_branch', else_branch'), then_items @ else_items
  | CFun _ ->
    (* Unreachable: lambdas are processed by hoist_aexpr. *)
    assert false

(* Don't touch the group, just hoist what's inside its bodies. *)
and hoist_bindings ctx bindings =
  let rec loop acc_bindings acc_items = function
    | [] -> List.rev acc_bindings, acc_items
    | (name, value) :: rest ->
      (match value with
       | CFun (params, body) ->
         let body', body_items = hoist_aexpr ctx body in
         loop ((name, CFun (params, body')) :: acc_bindings) (acc_items @ body_items) rest
       | _ ->
         let value', value_items = hoist_cexpr ctx value in
         loop ((name, value') :: acc_bindings) (acc_items @ value_items) rest)
  in
  loop [] [] bindings
;;

let hoist_item ctx = function
  | AValue (name, ACExpr (CFun (params, body))) ->
    let body', items = hoist_aexpr ctx body in
    items @ [ AValue (name, ACExpr (CFun (params, body'))) ]
  | AValue (name, body) ->
    let body', items = hoist_aexpr ctx body in
    items @ [ AValue (name, body') ]
  | ARec bindings ->
    let bindings', items = hoist_bindings ctx bindings in
    items @ [ ARec bindings' ]
  | AEval body ->
    let body', items = hoist_aexpr ctx body in
    items @ [ AEval body' ]
;;

let program program =
  let program = List.map simplify_item program in
  let gen = Fresh.create_from_program program in
  let ctx = { gen; counts = binder_counts program } in
  List.concat_map (hoist_item ctx) program
;;
