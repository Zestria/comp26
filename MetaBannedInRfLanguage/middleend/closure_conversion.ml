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

type ctx =
  { locals : SSet.t
  ; gen : Fresh.t
  }

let with_local ctx name = { ctx with locals = SSet.add name ctx.locals }
let with_locals ctx names = List.fold_left with_local ctx names
let imm_vars names = List.map (fun name -> ImmVar name) names

let caps_of ctx ~params body =
  SSet.elements (SSet.inter (SSet.diff (fv_aexpr body) (SSet.of_list params)) ctx.locals)
;;

let rec cc_cexpr ctx = function
  | (CImm _ | CApp _ | CTuple _ | CMatchFailure) as cexpr -> cexpr
  | CFun _ ->
    (* Shouldn't happen: ANF never puts a lambda in cexpr position. *)
    assert false
  | CIte (cond, then_branch, else_branch) ->
    CIte (cond, cc_aexpr ctx then_branch, cc_aexpr ctx else_branch)

and cc_aexpr ctx = function
  | ACExpr (CFun (params, body)) -> convert_tail ctx params body
  | ACExpr cexpr -> ACExpr (cc_cexpr ctx cexpr)
  | ALet (name, CFun (params, value), body) -> convert_let ctx name params value body
  | ALet (name, value, body) ->
    ALet (name, cc_cexpr ctx value, cc_aexpr (with_local ctx name) body)
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
    ALetTuple (names, subject, cc_aexpr ctx body)
  | ALetRec (bindings, body) ->
    let ctx_body = with_locals ctx (List.map fst bindings) in
    ALetRec (convert_rec ctx bindings, cc_aexpr ctx_body body)

and convert_let ctx name params value body =
  let caps = caps_of ctx ~params value in
  let value' = cc_aexpr (with_locals ctx params) value in
  let body' = cc_aexpr (with_local ctx name) body in
  if caps = []
  then ALet (name, CFun (params, value'), body')
  else (
    let helper = Fresh.fresh ~suffix:"__cc" ctx.gen name in
    ALet
      ( helper
      , CFun (caps @ params, value')
      , ALet (name, CApp (ImmVar helper, imm_vars caps), body') ))

and convert_tail ctx params value =
  let caps = caps_of ctx ~params value in
  let value' = cc_aexpr (with_locals ctx params) value in
  if caps = []
  then ACExpr (CFun (params, value'))
  else (
    let helper = Fresh.fresh ~suffix:"__cc" ctx.gen "f" in
    ALet
      (helper, CFun (caps @ params, value'), ACExpr (CApp (ImmVar helper, imm_vars caps))))

and convert_rec ctx bindings =
  let group =
    List.fold_left (fun acc (name, _) -> SSet.add name acc) SSet.empty bindings
  in
  let ctx_body = { ctx with locals = SSet.union ctx.locals group } in
  let convert_binding (name, value) =
    match value with
    | CFun (params, value_body) ->
      let caps = caps_of ctx ~params value_body in
      let value_body' = cc_aexpr (with_locals ctx_body params) value_body in
      if caps = []
      then [ name, CFun (params, value_body') ]
      else (
        let helper = Fresh.fresh ~suffix:"__cc" ctx.gen name in
        [ name, CFun (params, ACExpr (CApp (ImmVar helper, imm_vars caps)))
        ; helper, CFun (caps @ params, value_body')
        ])
    | _ -> [ name, cc_cexpr ctx_body value ]
  in
  List.concat_map convert_binding bindings
;;

let cc_item ctx = function
  | AValue (name, body) -> AValue (name, cc_aexpr ctx body)
  | ARec bindings -> ARec (convert_rec ctx bindings)
  | AEval body -> AEval (cc_aexpr ctx body)
;;

let program program =
  let gen = Fresh.create_from_program program in
  let ctx = { locals = SSet.empty; gen } in
  List.map (cc_item ctx) program
;;
