[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module SSet = Set.Make (String)

(* ---------- free variables ---------- *)

let fv_imm = function
  | ImmVar name -> SSet.singleton name
  | ImmInt _ | ImmBool _ | ImmUnit -> SSet.empty
;;

let fv_imms imms =
  List.fold_left (fun acc imm -> SSet.union acc (fv_imm imm)) SSet.empty imms
;;

let rec fv_cexpr = function
  | CImm imm -> fv_imm imm
  | CApp (fn, args) -> fv_imms (fn :: args)
  | CTuple (first, second, rest) -> fv_imms (first :: second :: rest)
  | CIte (cond, then_branch, else_branch) ->
    SSet.union (fv_imm cond) (SSet.union (fv_aexpr then_branch) (fv_aexpr else_branch))
  | CFun (params, body) -> SSet.diff (fv_aexpr body) (SSet.of_list params)
  | CMatchFailure -> SSet.empty

and fv_aexpr = function
  | ACExpr cexpr -> fv_cexpr cexpr
  | ALet (name, value, body) ->
    SSet.union (fv_cexpr value) (SSet.remove name (fv_aexpr body))
  | ALetTuple (names, subject, body) ->
    let bound =
      List.fold_left
        (fun acc name ->
           match name with
           | Some name -> SSet.add name acc
           | None -> acc)
        SSet.empty
        names
    in
    SSet.union (fv_imm subject) (SSet.diff (fv_aexpr body) bound)
  | ALetRec (bindings, body) ->
    let names =
      List.fold_left (fun acc (name, _) -> SSet.add name acc) SSet.empty bindings
    in
    let fv =
      List.fold_left
        (fun acc (_, value) -> SSet.union acc (fv_cexpr value))
        (fv_aexpr body)
        bindings
    in
    SSet.diff fv names
;;

(* ---------- conversion ---------- *)

let ( let* ) = Fresh.( let* )
let return = Fresh.return

type ctx = { locals : SSet.t }

let with_local ctx name = { locals = SSet.add name ctx.locals }
let with_locals ctx names = List.fold_left with_local ctx names
let imm_vars names = List.map (fun name -> ImmVar name) names

let caps_of ctx ~params body =
  SSet.elements (SSet.inter (SSet.diff (fv_aexpr body) (SSet.of_list params)) ctx.locals)
;;

let rec cc_cexpr ctx = function
  | CIte (cond, then_branch, else_branch) ->
    let* then_branch = cc_aexpr ctx then_branch in
    let* else_branch = cc_aexpr ctx else_branch in
    return (CIte (cond, then_branch, else_branch))
  | (CImm _ | CApp _ | CTuple _ | CMatchFailure | CFun _) as cexpr ->
    (* A lambda never gets here: [cc_aexpr] and [convert_rec] convert it first,
       and ANF never puts a lambda in any other cexpr position. *)
    return cexpr

and cc_aexpr ctx = function
  | ACExpr (CFun (params, body)) -> convert_tail ctx params body
  | ACExpr cexpr ->
    let* cexpr = cc_cexpr ctx cexpr in
    return (ACExpr cexpr)
  | ALet (name, CFun (params, value), body) -> convert_let ctx name params value body
  | ALet (name, value, body) ->
    let* value = cc_cexpr ctx value in
    let* body = cc_aexpr (with_local ctx name) body in
    return (ALet (name, value, body))
  | ALetTuple (names, subject, body) ->
    let ctx =
      List.fold_left
        (fun ctx name ->
           match name with
           | Some name -> with_local ctx name
           | None -> ctx)
        ctx
        names
    in
    let* body = cc_aexpr ctx body in
    return (ALetTuple (names, subject, body))
  | ALetRec (bindings, body) ->
    let ctx_body = with_locals ctx (List.map fst bindings) in
    let* bindings = convert_rec ctx bindings in
    let* body = cc_aexpr ctx_body body in
    return (ALetRec (bindings, body))

and convert_let ctx name params value body =
  let* value' = cc_aexpr (with_locals ctx params) value in
  let* body' = cc_aexpr (with_local ctx name) body in
  match caps_of ctx ~params value with
  | [] -> return (ALet (name, CFun (params, value'), body'))
  | caps ->
    let* helper = Fresh.name ~suffix:"__cc" name in
    return
      (ALet
         ( helper
         , CFun (caps @ params, value')
         , ALet (name, CApp (ImmVar helper, imm_vars caps), body') ))

and convert_tail ctx params value =
  let* value' = cc_aexpr (with_locals ctx params) value in
  match caps_of ctx ~params value with
  | [] -> return (ACExpr (CFun (params, value')))
  | caps ->
    let* helper = Fresh.name ~suffix:"__cc" "f" in
    return
      (ALet
         ( helper
         , CFun (caps @ params, value')
         , ACExpr (CApp (ImmVar helper, imm_vars caps)) ))

and convert_rec ctx bindings =
  let group =
    List.fold_left (fun acc (name, _) -> SSet.add name acc) SSet.empty bindings
  in
  let ctx_body = { locals = SSet.union ctx.locals group } in
  let convert_binding (name, value) =
    match value with
    | CFun (params, value_body) ->
      let* value_body' = cc_aexpr (with_locals ctx_body params) value_body in
      (match caps_of ctx ~params value_body with
       | [] -> return [ name, CFun (params, value_body') ]
       | caps ->
         let* helper = Fresh.name ~suffix:"__cc" name in
         return
           [ name, CFun (params, ACExpr (CApp (ImmVar helper, imm_vars (caps @ params))))
           ; helper, CFun (caps @ params, value_body')
           ])
    | _ ->
      let* value = cc_cexpr ctx_body value in
      return [ name, value ]
  in
  let* converted = Fresh.map_list convert_binding bindings in
  return (List.concat converted)
;;

let cc_item ctx = function
  | AValue (name, body) ->
    let* body = cc_aexpr ctx body in
    return (AValue (name, body))
  | ARec bindings ->
    let* bindings = convert_rec ctx bindings in
    return (ARec bindings)
  | AEval body ->
    let* body = cc_aexpr ctx body in
    return (AEval body)
;;

let program program =
  let ctx = { locals = SSet.empty } in
  Fresh.run (Fresh.create_from_program program) (Fresh.map_list (cc_item ctx) program)
;;
