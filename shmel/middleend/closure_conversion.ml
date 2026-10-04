[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Anf
module Names = Set.Make (String)

let fv_imm = function
  | A.ImmInt _ -> Names.empty
  | A.ImmVar x -> Names.singleton x
;;

let union = Names.union
let without xs names = Names.diff names (Names.of_list xs)

let fv_cond = function
  | A.CondImm i -> fv_imm i
  | A.CondCmp (_, a, b) -> union (fv_imm a) (fv_imm b)
;;

let rec fv = function
  | A.AExpr c -> fv_c c
  | A.ALet (x, c, rest) -> union (fv_c c) (Names.remove x (fv rest))
  | A.ALetRec (f, ps, body, rest) ->
    union (without (f :: ps) (fv body)) (Names.remove f (fv rest))

and fv_c = function
  | A.CImm i | A.CNeg i -> fv_imm i
  | A.CBinOp (_, a, b) -> union (fv_imm a) (fv_imm b)
  | A.CApp (f, args) -> List.fold_left (fun s i -> union s (fv_imm i)) (fv_imm f) args
  | A.CIf (c, t, e) -> union (fv_cond c) (union (fv t) (fv e))
  | A.CFun (ps, body) -> without ps (fv body)
;;

let free_variables e = Names.elements (fv e)

let rec names_expr = function
  | A.AExpr c -> names_complex c
  | A.ALet (x, c, rest) -> Names.add x (union (names_complex c) (names_expr rest))
  | A.ALetRec (f, ps, body, rest) ->
    union (Names.of_list (f :: ps)) (union (names_expr body) (names_expr rest))

and names_complex = function
  | A.CFun (ps, body) -> union (Names.of_list ps) (names_expr body)
  | A.CIf (c, t, e) -> union (fv_cond c) (union (names_expr t) (names_expr e))
  | c -> fv_c c
;;

module Env = Map.Make (String)

let of_program program =
  let names_decl = function
    | A.DExpr e -> names_expr e
    | A.DLet (x, ps, e) | A.DLetRec (x, ps, e) ->
      union (Names.of_list (x :: ps)) (names_expr e)
  in
  let used =
    ref (List.fold_left (fun s d -> union s (names_decl d)) Names.empty program)
  in
  let fresh prefix =
    let rec find n =
      let name = prefix ^ Int.to_string n in
      if Names.mem name !used
      then find (n + 1)
      else (
        used := Names.add name !used;
        name)
    in
    find 0
  in
  let immediate env = function
    | A.ImmInt _ as i -> i
    | A.ImmVar x -> A.ImmVar (Option.value (Env.find_opt x env) ~default:x)
  in
  let application f = function
    | [] -> A.CImm (A.ImmVar f)
    | args -> A.CApp (A.ImmVar f, args)
  in
  let hide env names = List.fold_left (fun env x -> Env.remove x env) env names in
  let condition env = function
    | A.CondImm i -> A.CondImm (immediate env i)
    | A.CondCmp (op, a, b) -> A.CondCmp (op, immediate env a, immediate env b)
  in
  let rec expr env = function
    | A.AExpr c -> complex env c (fun c -> A.AExpr c)
    | A.ALet (x, c, rest) ->
      complex env c (fun c -> A.ALet (x, c, expr (hide env [ x ]) rest))
    | A.ALetRec (f, params, body, rest) ->
      function_value env (Some f) params body (fun c ->
        A.ALet (f, c, expr (hide env [ f ]) rest))
  and complex env c k =
    match c with
    | A.CImm i -> k (A.CImm (immediate env i))
    | A.CNeg i -> k (A.CNeg (immediate env i))
    | A.CBinOp (op, a, b) -> k (A.CBinOp (op, immediate env a, immediate env b))
    | A.CApp (f, args) -> k (A.CApp (immediate env f, List.map (immediate env) args))
    | A.CIf (c, t, e) ->
      let t = expr env t in
      let e = expr env e in
      k (A.CIf (condition env c, t, e))
    | A.CFun (params, body) -> function_value env None params body k
  and function_value env recursive params body k =
    let helper = fresh "cc_fun" in
    let bound =
      match recursive with
      | None -> params
      | Some f -> f :: params
    in
    let captures = Names.elements (without bound (fv body)) in
    let capture_params = List.map (fun _ -> fresh "cc_capture") captures in
    let body_env =
      List.fold_left2
        (fun env x p -> Env.add x p env)
        (hide env bound)
        captures
        capture_params
    in
    let recursive_alias =
      match recursive with
      | Some f when (not (List.mem f params)) && Names.mem f (fv body) ->
        Some (f, fresh "cc_rec")
      | _ -> None
    in
    let body_env =
      match recursive_alias with
      | None -> body_env
      | Some (f, alias) -> Env.add f alias body_env
    in
    let converted = expr body_env body in
    let converted =
      match recursive_alias with
      | None -> converted
      | Some (_, alias) ->
        A.ALet
          ( alias
          , application helper (List.map (fun p -> A.ImmVar p) capture_params)
          , converted )
    in
    let rest =
      k (application helper (List.map (fun x -> immediate env (A.ImmVar x)) captures))
    in
    let all_params = capture_params @ params in
    match recursive with
    | None -> A.ALet (helper, A.CFun (all_params, converted), rest)
    | Some _ -> A.ALetRec (helper, all_params, converted, rest)
  in
  List.map
    (function
      | A.DExpr e -> A.DExpr (expr Env.empty e)
      | A.DLet (x, [], e) -> A.DLet (x, [], expr Env.empty e)
      | A.DLet (x, params, body) ->
        A.DLet (x, [], function_value Env.empty None params body (fun c -> A.AExpr c))
      | A.DLetRec (f, params, body) ->
        A.DLet (f, [], function_value Env.empty (Some f) params body (fun c -> A.AExpr c)))
    program
;;
