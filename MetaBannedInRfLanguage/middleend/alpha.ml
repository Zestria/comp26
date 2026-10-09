[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SMap = Map.Make (String)

let rec idents_pattern acc = function
  | Ast.PAny | Ast.PConstant _ -> acc
  | Ast.PVar name -> Fresh.SSet.add name acc
  | Ast.PTuple (first, second, rest) -> List.fold_left idents_pattern acc (first :: second :: rest)
;;

let rec idents_expr acc = function
  | Ast.EConst _ -> acc
  | Ast.EIdent name -> Fresh.SSet.add name acc
  | Ast.EFun (pat, body) -> idents_expr (idents_pattern acc pat) body
  | Ast.EApply (fn, arg) -> idents_expr (idents_expr acc fn) arg
  | Ast.ETuple (first, second, rest) -> List.fold_left idents_expr acc (first :: second :: rest)
  | Ast.EIf (cond, then_branch, else_branch) ->
    List.fold_left idents_expr acc (cond :: then_branch :: Option.to_list else_branch)
  | Ast.ELet (_, first, rest, body) ->
    let bindings = first :: rest in
    let acc =
      List.fold_left
        (fun acc { Ast.vb_pat; vb_expr } -> idents_expr (idents_pattern acc vb_pat) vb_expr)
        acc
        bindings
    in
    idents_expr acc body
  | Ast.EFunction (first, rest) -> List.fold_left idents_case acc (first :: rest)
  | Ast.EMatch (subject, first, rest) ->
    List.fold_left idents_case (idents_expr acc subject) (first :: rest)

and idents_case acc { Ast.case_pat; case_expr } = idents_expr (idents_pattern acc case_pat) case_expr
;;

let idents_item acc = function
  | Ast.SEval expr -> idents_expr acc expr
  | Ast.SValue (_, first, rest) ->
    List.fold_left
      (fun acc { Ast.vb_pat; vb_expr } -> idents_expr (idents_pattern acc vb_pat) vb_expr)
      acc
      (first :: rest)
;;

let structure_idents structure = List.fold_left idents_item Fresh.SSet.empty structure
let expression_idents expr = idents_expr Fresh.SSet.empty expr

let lookup env name = Option.value (SMap.find_opt name env) ~default:name

let rec rename_pattern fresh env = function
  | Ast.PAny -> env, Ast.PAny
  | Ast.PConstant _ as pat -> env, pat
  | Ast.PVar name ->
    let renamed = Fresh.fresh fresh name in
    SMap.add name renamed env, Ast.PVar renamed
  | Ast.PTuple (first, second, rest) ->
    let env, first = rename_pattern fresh env first in
    let env, second = rename_pattern fresh env second in
    let env, rest =
      List.fold_left
        (fun (env, acc) pat ->
           let env, pat = rename_pattern fresh env pat in
           env, pat :: acc)
        (env, [])
        rest
    in
    env, Ast.PTuple (first, second, List.rev rest)
;;

let rec rename_expr fresh env expr =
  match expr with
  | Ast.EConst _ -> expr
  | Ast.EIdent name -> Ast.EIdent (lookup env name)
  | Ast.EFun (pat, body) ->
    let env, pat = rename_pattern fresh env pat in
    Ast.EFun (pat, rename_expr fresh env body)
  | Ast.EApply (fn, arg) -> Ast.EApply (rename_expr fresh env fn, rename_expr fresh env arg)
  | Ast.ETuple (first, second, rest) ->
    Ast.ETuple
      ( rename_expr fresh env first
      , rename_expr fresh env second
      , List.map (rename_expr fresh env) rest )
  | Ast.EIf (cond, then_branch, else_branch) ->
    Ast.EIf
      ( rename_expr fresh env cond
      , rename_expr fresh env then_branch
      , Option.map (rename_expr fresh env) else_branch )
  | Ast.ELet (NonRec, first, rest, body) ->
    let bindings = first :: rest in
    let renamed_exprs = List.map (fun { Ast.vb_expr; _ } -> rename_expr fresh env vb_expr) bindings in
    let env, renamed_pats_rev =
      List.fold_left
        (fun (env, acc) { Ast.vb_pat; _ } ->
           let env, pat = rename_pattern fresh env vb_pat in
           env, pat :: acc)
        (env, [])
        bindings
    in
    let renamed_pats = List.rev renamed_pats_rev in
    let renamed_bindings =
      List.map2 (fun vb_pat vb_expr -> { Ast.vb_pat; vb_expr }) renamed_pats renamed_exprs
    in
    let first, rest =
      match renamed_bindings with
      | first :: rest -> first, rest
      | [] -> assert false
    in
    Ast.ELet (NonRec, first, rest, rename_expr fresh env body)
  | Ast.ELet (Rec, first, rest, body) ->
    let bindings = first :: rest in
    let env, renamed_pats_rev =
      List.fold_left
        (fun (env, acc) { Ast.vb_pat; _ } ->
           let env, pat = rename_pattern fresh env vb_pat in
           env, pat :: acc)
        (env, [])
        bindings
    in
    let renamed_pats = List.rev renamed_pats_rev in
    let renamed_bindings =
      List.map2
        (fun vb_pat { Ast.vb_expr; _ } -> { Ast.vb_pat; vb_expr = rename_expr fresh env vb_expr })
        renamed_pats
        bindings
    in
    let first, rest =
      match renamed_bindings with
      | first :: rest -> first, rest
      | [] -> assert false
    in
    Ast.ELet (Rec, first, rest, rename_expr fresh env body)
  | Ast.EFunction (first, rest) ->
    let rename_case { Ast.case_pat; case_expr } =
      let env, case_pat = rename_pattern fresh env case_pat in
      { Ast.case_pat; case_expr = rename_expr fresh env case_expr }
    in
    (match List.map rename_case (first :: rest) with
     | first :: rest -> Ast.EFunction (first, rest)
     | [] -> assert false)
  | Ast.EMatch (subject, first, rest) ->
    let subject = rename_expr fresh env subject in
    let rename_case { Ast.case_pat; case_expr } =
      let env, case_pat = rename_pattern fresh env case_pat in
      { Ast.case_pat; case_expr = rename_expr fresh env case_expr }
    in
    (match List.map rename_case (first :: rest) with
     | first :: rest -> Ast.EMatch (subject, first, rest)
     | [] -> assert false)
;;

let rename_item fresh env = function
  | Ast.SEval expr -> env, Ast.SEval (rename_expr fresh env expr)
  | Ast.SValue (NonRec, first, rest) ->
    let bindings = first :: rest in
    let renamed_exprs = List.map (fun { Ast.vb_expr; _ } -> rename_expr fresh env vb_expr) bindings in
    let env, renamed_pats_rev =
      List.fold_left
        (fun (env, acc) { Ast.vb_pat; _ } ->
           let env, pat = rename_pattern fresh env vb_pat in
           env, pat :: acc)
        (env, [])
        bindings
    in
    let renamed_pats = List.rev renamed_pats_rev in
    let renamed_bindings =
      List.map2 (fun vb_pat vb_expr -> { Ast.vb_pat; vb_expr }) renamed_pats renamed_exprs
    in
    let first, rest =
      match renamed_bindings with
      | first :: rest -> first, rest
      | [] -> assert false
    in
    env, Ast.SValue (NonRec, first, rest)
  | Ast.SValue (Rec, first, rest) ->
    let bindings = first :: rest in
    let env, renamed_pats_rev =
      List.fold_left
        (fun (env, acc) { Ast.vb_pat; _ } ->
           let env, pat = rename_pattern fresh env vb_pat in
           env, pat :: acc)
        (env, [])
        bindings
    in
    let renamed_pats = List.rev renamed_pats_rev in
    let renamed_bindings =
      List.map2
        (fun vb_pat { Ast.vb_expr; _ } -> { Ast.vb_pat; vb_expr = rename_expr fresh env vb_expr })
        renamed_pats
        bindings
    in
    let first, rest =
      match renamed_bindings with
      | first :: rest -> first, rest
      | [] -> assert false
    in
    env, Ast.SValue (Rec, first, rest)
;;

let rename_structure structure =
  let fresh = Fresh.create ~taken:(structure_idents structure) in
  let rec go env = function
    | [] -> []
    | item :: rest ->
      let env, item = rename_item fresh env item in
      item :: go env rest
  in
  go SMap.empty structure
;;

let rename_expression expr =
  let fresh = Fresh.create ~taken:(expression_idents expr) in
  rename_expr fresh SMap.empty expr
;;
