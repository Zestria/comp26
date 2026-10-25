[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module SSet = Set.Make (String)

let pure_prims =
  [ "+"; "-"; "~+"; "~-"; "*"; "<"; "<="; ">"; ">="; "="; "<>"; "&&"; "||"; "not" ]
;;

let fv_imm = function
  | ImmVar name -> SSet.singleton name
  | ImmInt _ | ImmBool _ | ImmUnit -> SSet.empty
;;

let fv_imms imms =
  List.fold_left (fun acc imm -> SSet.union acc (fv_imm imm)) SSet.empty imms
;;

(* A computation is pure if dropping it cannot change what the program does. *)
let rec pure_cexpr ~is_prim = function
  | CImm _ | CTuple _ | CFun _ -> true
  | CApp (ImmVar op, _) when is_prim op && List.mem op pure_prims -> true
  | CApp (ImmVar "/", [ _; ImmInt divisor ]) when is_prim "/" && divisor <> 0 -> true
  | CApp _ | CMatchFailure -> false
  | CIte (_, then_branch, else_branch) ->
    pure_aexpr ~is_prim then_branch && pure_aexpr ~is_prim else_branch

and pure_aexpr ~is_prim = function
  | ALet (_, cexpr, body) -> pure_cexpr ~is_prim cexpr && pure_aexpr ~is_prim body
  | ALetTuple (_, _, body) | ALetRec (_, body) -> pure_aexpr ~is_prim body
  | ACExpr cexpr -> pure_cexpr ~is_prim cexpr
;;

(* Both functions return the simplified expression and its free variables. *)
let rec cexpr ~is_prim = function
  | CImm imm as c -> c, fv_imm imm
  | CMatchFailure -> CMatchFailure, SSet.empty
  | CApp (fn, args) as c -> c, fv_imms (fn :: args)
  | CTuple (first, second, rest) as c -> c, fv_imms (first :: second :: rest)
  | CIte (cond, then_branch, else_branch) ->
    let then_branch, fv_then = aexpr ~is_prim then_branch in
    let else_branch, fv_else = aexpr ~is_prim else_branch in
    ( CIte (cond, then_branch, else_branch)
    , SSet.union (fv_imm cond) (SSet.union fv_then fv_else) )
  | CFun (params, body) ->
    let body, fv_body = aexpr ~is_prim body in
    CFun (params, body), SSet.diff fv_body (SSet.of_list params)

and aexpr ~is_prim = function
  | ALet (name, value, body) ->
    let value, fv_value = cexpr ~is_prim value in
    let body, fv_body = aexpr ~is_prim body in
    (match body with
     | ACExpr (CImm (ImmVar result)) when String.equal result name ->
       ACExpr value, fv_value
     | _ when (not (SSet.mem name fv_body)) && pure_cexpr ~is_prim value -> body, fv_body
     | _ -> ALet (name, value, body), SSet.union fv_value (SSet.remove name fv_body))
  | ALetTuple (names, subject, body) ->
    let body, fv_body = aexpr ~is_prim body in
    let names =
      List.map
        (function
          | Some name when SSet.mem name fv_body -> Some name
          | _ -> None)
        names
    in
    if List.for_all Option.is_none names
    then body, fv_body
    else (
      let bound = SSet.of_list (List.filter_map Fun.id names) in
      ( ALetTuple (names, subject, body)
      , SSet.union (fv_imm subject) (SSet.diff fv_body bound) ))
  | ALetRec (bindings, body) ->
    let body, fv_body = aexpr ~is_prim body in
    let names = SSet.of_list (List.map fst bindings) in
    if SSet.disjoint names fv_body
    then body, fv_body
    else (
      let bindings, fvs =
        List.split
          (List.map
             (fun (name, value) ->
                let value, fv_value = cexpr ~is_prim value in
                (name, value), fv_value)
             bindings)
      in
      let fv = List.fold_left SSet.union fv_body fvs in
      ALetRec (bindings, body), SSet.diff fv names)
  | ACExpr value ->
    let value, fv_value = cexpr ~is_prim value in
    ACExpr value, fv_value
;;

let item ~is_prim = function
  | AValue (name, body) -> AValue (name, fst (aexpr ~is_prim body))
  | ARec bindings ->
    ARec (List.map (fun (name, value) -> name, fst (cexpr ~is_prim value)) bindings)
  | AEval body -> AEval (fst (aexpr ~is_prim body))
;;

let program ~is_prim program = List.map (item ~is_prim) program
