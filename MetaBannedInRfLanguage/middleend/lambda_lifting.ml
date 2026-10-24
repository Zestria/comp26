[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module SMap = Map.Make (String)

let bump counts name =
  SMap.update name (fun count -> Some (Option.value count ~default:0 + 1)) counts
;;

let binder_counts program =
  let rec cexpr counts = function
    | CImm _ | CApp _ | CTuple _ | CMatchFailure -> counts
    | CFun (params, body) -> aexpr (List.fold_left bump counts params) body
    | CIte (_, then_branch, else_branch) -> aexpr (aexpr counts then_branch) else_branch
  and aexpr counts = function
    | ACExpr expr -> cexpr counts expr
    | ALet (name, value, body) -> aexpr (bump (cexpr counts value) name) body
    | ALetTuple (names, _, body) ->
      let counts =
        List.fold_left
          (fun counts name -> Option.fold ~none:counts ~some:(bump counts) name)
          counts
          names
      in
      aexpr counts body
    | ALetRec (bindings, body) -> aexpr (List.fold_left binding counts bindings) body
  and binding counts (name, value) = cexpr (bump counts name) value in
  let item counts = function
    | AValue (name, body) -> aexpr (bump counts name) body
    | ARec bindings -> List.fold_left binding counts bindings
    | AEval body -> aexpr counts body
  in
  List.fold_left item SMap.empty program
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

let ( let* ) = Fresh.( let* )
let return = Fresh.return

type ctx = { counts : int SMap.t }

let can_reuse ctx name = Option.value (SMap.find_opt name ctx.counts) ~default:0 = 1

let rec hoist_aexpr ctx = function
  | ALet (name, CFun (params, body), cont) ->
    let* body', body_items = hoist_aexpr ctx body in
    let* cont', cont_items = hoist_aexpr ctx cont in
    let* lifted, binding =
      if can_reuse ctx name
      then return (name, cont')
      else
        let* fresh = Fresh.name ~suffix:"__ll" name in
        return (fresh, ALet (name, CImm (ImmVar fresh), cont'))
    in
    return
      ( binding
      , body_items @ [ AValue (lifted, ACExpr (CFun (params, body'))) ] @ cont_items )
  | ALet (name, value, cont) ->
    let* value', value_items = hoist_cexpr ctx value in
    let* cont', cont_items = hoist_aexpr ctx cont in
    return (ALet (name, value', cont'), value_items @ cont_items)
  | ACExpr (CFun (params, body)) ->
    let* body', body_items = hoist_aexpr ctx body in
    let* fresh = Fresh.name ~suffix:"__ll" "f" in
    return
      ( ACExpr (CImm (ImmVar fresh))
      , body_items @ [ AValue (fresh, ACExpr (CFun (params, body'))) ] )
  | ACExpr value ->
    let* value', value_items = hoist_cexpr ctx value in
    return (ACExpr value', value_items)
  | ALetTuple (names, subject, cont) ->
    let* cont', cont_items = hoist_aexpr ctx cont in
    return (ALetTuple (names, subject, cont'), cont_items)
  | ALetRec (bindings, cont) ->
    let* bindings', body_items = hoist_bindings ctx bindings in
    let* cont', cont_items = hoist_aexpr ctx cont in
    return (ALetRec (bindings', cont'), body_items @ cont_items)

and hoist_cexpr ctx = function
  | CIte (cond, then_branch, else_branch) ->
    let* then_branch', then_items = hoist_aexpr ctx then_branch in
    let* else_branch', else_items = hoist_aexpr ctx else_branch in
    return (CIte (cond, then_branch', else_branch'), then_items @ else_items)
  | (CImm _ | CApp _ | CTuple _ | CMatchFailure | CFun _) as value ->
    (* A lambda never gets here: [hoist_aexpr] and [hoist_bindings] lift it
       first, and ANF never puts a lambda in any other cexpr position. *)
    return (value, [])

(* Don't touch the group, just hoist what's inside its bodies. *)
and hoist_bindings ctx bindings =
  let hoist_binding (name, value) =
    match value with
    | CFun (params, body) ->
      let* body', items = hoist_aexpr ctx body in
      return ((name, CFun (params, body')), items)
    | _ ->
      let* value', items = hoist_cexpr ctx value in
      return ((name, value'), items)
  in
  let* hoisted = Fresh.map_list hoist_binding bindings in
  return (List.map fst hoisted, List.concat_map snd hoisted)
;;

let hoist_item ctx = function
  | AValue (name, ACExpr (CFun (params, body))) ->
    let* body', items = hoist_aexpr ctx body in
    return (items @ [ AValue (name, ACExpr (CFun (params, body'))) ])
  | AValue (name, body) ->
    let* body', items = hoist_aexpr ctx body in
    return (items @ [ AValue (name, body') ])
  | ARec bindings ->
    let* bindings', items = hoist_bindings ctx bindings in
    return (items @ [ ARec bindings' ])
  | AEval body ->
    let* body', items = hoist_aexpr ctx body in
    return (items @ [ AEval body' ])
;;

let program program =
  let program = List.map simplify_item program in
  let ctx = { counts = binder_counts program } in
  let hoisted =
    Fresh.run
      (Fresh.create_from_program program)
      (Fresh.map_list (hoist_item ctx) program)
  in
  List.concat hoisted
;;
