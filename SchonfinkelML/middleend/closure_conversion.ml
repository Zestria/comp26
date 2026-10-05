[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module VarSet = Set.Make (String)

let fv_imm : immexpr -> VarSet.t = function
  | ImmId x -> VarSet.singleton x
  | ImmNum _ | ImmBool _ | ImmUnit -> VarSet.empty
;;

let rec fv_cexpr : cexpr -> VarSet.t = function
  | CBinop (_, a, b) -> VarSet.union (fv_imm a) (fv_imm b)
  | CImmExpr i -> fv_imm i
  | CIfExpr (i, t, e) -> VarSet.union (fv_imm i) (VarSet.union (fv_aexpr t) (fv_aexpr e))
  | CApp (f, a, args) ->
    List.fold_left
      (fun acc x -> VarSet.union acc (fv_imm x))
      (VarSet.union (fv_imm f) (fv_imm a))
      args
  | CAbstr (p, body) -> VarSet.remove p (fv_aexpr body)

and fv_aexpr : aexpr -> VarSet.t = function
  | ALet (flag, name, rhs, rest) ->
    let fv_rhs = fv_aexpr rhs in
    let fv_rhs = if flag = Recursive then VarSet.remove name fv_rhs else fv_rhs in
    VarSet.union fv_rhs (VarSet.remove name (fv_aexpr rest))
  | ACExpr c -> fv_cexpr c
;;

let free_vars a = VarSet.elements (fv_aexpr a)

let rec idents_cexpr acc : cexpr -> VarSet.t = function
  | CBinop (_, a, b) -> idents_imm (idents_imm acc a) b
  | CImmExpr i -> idents_imm acc i
  | CIfExpr (i, t, e) -> idents_aexpr (idents_aexpr (idents_imm acc i) t) e
  | CApp (f, a, args) -> List.fold_left idents_imm (idents_imm (idents_imm acc f) a) args
  | CAbstr (p, body) -> idents_aexpr (VarSet.add p acc) body

and idents_imm acc : immexpr -> VarSet.t = function
  | ImmId x -> VarSet.add x acc
  | ImmNum _ | ImmBool _ | ImmUnit -> acc

and idents_aexpr acc : aexpr -> VarSet.t = function
  | ALet (_, name, rhs, rest) ->
    idents_aexpr (idents_aexpr (VarSet.add name acc) rhs) rest
  | ACExpr c -> idents_cexpr acc c
;;

type naming =
  { reserved : VarSet.t
  ; counter : int
  }

let naming_of a = { reserved = idents_aexpr VarSet.empty a; counter = 0 }

let gensym (naming : naming) ~prefix () =
  let rec pick counter =
    let name = Printf.sprintf "%s_clo%d" prefix counter in
    if VarSet.mem name naming.reserved then pick (counter + 1) else name, counter
  in
  let name, counter = pick (naming.counter + 1) in
  name, { reserved = VarSet.add name naming.reserved; counter }
;;

let rec collect_params = function
  | ACExpr (CAbstr (p, body)) ->
    let ps, inner = collect_params body in
    p :: ps, inner
  | other -> [], other
;;

let rec build_abstr params body =
  match params with
  | [] -> body
  | [ p ] -> ACExpr (CAbstr (p, body))
  | p :: rest -> ACExpr (CAbstr (p, build_abstr rest body))
;;

type rename_ctx = (string * string) list
type self_ctx = (string * string list) list

let apply_rename (rename_ctx : rename_ctx) x =
  match List.assoc_opt x rename_ctx with
  | Some x' -> x'
  | None -> x
;;

let convert_imm (rename_ctx : rename_ctx) = function
  | ImmId x -> ImmId (apply_rename rename_ctx x)
  | other -> other
;;

let hide_rename (rename_ctx : rename_ctx) names =
  List.filter (fun (k, _) -> not (List.mem k names)) rename_ctx
;;

let extend_self_call (self_ctx : self_ctx) fname f' a' (args' : immexpr list) =
  match List.assoc_opt fname self_ctx with
  | Some (fv :: fvs) ->
    CApp (f', ImmId fv, List.map (fun v -> ImmId v) fvs @ (a' :: args'))
  | Some [] | None -> CApp (f', a', args')
;;

let rec convert_aexpr
          (naming : naming)
          (self_ctx : self_ctx)
          (rename_ctx : rename_ctx)
          (a : aexpr)
  : naming * aexpr
  =
  match a with
  | ACExpr (CAbstr _) ->
    let params, inner_body = collect_params a in
    let rename_ctx_inner = hide_rename rename_ctx params in
    let naming, inner_body' = convert_aexpr naming self_ctx rename_ctx_inner inner_body in
    let bound = VarSet.of_list params in
    let fv_list = VarSet.elements (VarSet.diff (fv_aexpr inner_body') bound) in
    (match fv_list with
     | [] -> naming, build_abstr params inner_body'
     | _ :: _ as fv_list ->
       let raw_name, naming = gensym naming ~prefix:"lam" () in
       let rhs' = build_abstr (fv_list @ params) inner_body' in
       let closure_app =
         match List.map (fun v -> ImmId v) fv_list with
         | first :: more -> ACExpr (CApp (ImmId raw_name, first, more))
         | [] -> assert false
       in
       naming, ALet (NotRecursive, raw_name, rhs', closure_app))
  | ACExpr c ->
    let naming, c' = convert_cexpr naming self_ctx rename_ctx c in
    naming, ACExpr c'
  | ALet (flag, name, rhs, rest) ->
    (match collect_params rhs with
     | [], _ ->
       let rename_ctx_rhs =
         if flag = Recursive then hide_rename rename_ctx [ name ] else rename_ctx
       in
       let naming, rhs' = convert_aexpr naming self_ctx rename_ctx_rhs rhs in
       let rename_ctx_rest = hide_rename rename_ctx [ name ] in
       let naming, rest' = convert_aexpr naming self_ctx rename_ctx_rest rest in
       naming, ALet (flag, name, rhs', rest')
     | params, inner_body ->
       let bound =
         let b = List.fold_left (fun acc p -> VarSet.add p acc) VarSet.empty params in
         if flag = Recursive then VarSet.add name b else b
       in
       let fv_orig = VarSet.elements (VarSet.diff (fv_aexpr inner_body) bound) in
       let fv_list = List.map (apply_rename rename_ctx) fv_orig in
       let rename_ctx_inner = hide_rename rename_ctx (VarSet.elements bound) in
       (match fv_orig with
        | [] ->
          let self_ctx' = if flag = Recursive then (name, []) :: self_ctx else self_ctx in
          let naming, inner_body' =
            convert_aexpr naming self_ctx' rename_ctx_inner inner_body
          in
          let rhs' = build_abstr params inner_body' in
          let rename_ctx_rest = hide_rename rename_ctx [ name ] in
          let naming, rest' = convert_aexpr naming self_ctx rename_ctx_rest rest in
          naming, ALet (flag, name, rhs', rest')
        | _ :: _ ->
          let self_ctx' = (name, fv_list) :: self_ctx in
          let naming, inner_body' =
            convert_aexpr naming self_ctx' rename_ctx_inner inner_body
          in
          let rhs' = build_abstr (fv_list @ params) inner_body' in
          let closed_name, naming = gensym naming ~prefix:name () in
          let closure_app =
            match List.map (fun v -> ImmId v) fv_list with
            | first :: more -> ACExpr (CApp (ImmId name, first, more))
            | [] -> assert false
          in
          let rename_ctx_rest = (name, closed_name) :: hide_rename rename_ctx [ name ] in
          let naming, rest' = convert_aexpr naming self_ctx rename_ctx_rest rest in
          ( naming
          , ALet (flag, name, rhs', ALet (NotRecursive, closed_name, closure_app, rest'))
          )))

and convert_cexpr
      (naming : naming)
      (self_ctx : self_ctx)
      (rename_ctx : rename_ctx)
      (c : cexpr)
  : naming * cexpr
  =
  match c with
  | CImmExpr i -> naming, CImmExpr (convert_imm rename_ctx i)
  | CBinop (op, a, b) ->
    naming, CBinop (op, convert_imm rename_ctx a, convert_imm rename_ctx b)
  | CIfExpr (i, t, e) ->
    let naming, t' = convert_aexpr naming self_ctx rename_ctx t in
    let naming, e' = convert_aexpr naming self_ctx rename_ctx e in
    naming, CIfExpr (convert_imm rename_ctx i, t', e')
  | CApp (f, a, args) ->
    let f' = convert_imm rename_ctx f in
    let a' = convert_imm rename_ctx a in
    let args' = List.map (convert_imm rename_ctx) args in
    (match f with
     | ImmId fname -> naming, extend_self_call self_ctx fname f' a' args'
     | _ -> naming, CApp (f', a', args'))
  | CAbstr _ ->
    failwith
      "Closure_conversion: bare CAbstr in cexpr position (intercepted earlier by \
       convert_aexpr)"
;;

let closure_convert (a : aexpr) : aexpr =
  let naming = naming_of a in
  snd (convert_aexpr naming [] [] a)
;;
