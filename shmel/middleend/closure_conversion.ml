[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Anf
module C = Closure_ir
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
  let rec expr = function
    | A.AExpr c -> C.AExpr (complex c)
    | A.ALet (x, c, rest) ->
      let c = complex c in
      C.ALet (x, c, expr rest)
    | A.ALetRec (f, ps, body, rest) ->
      let c = closure (Some f) ps body in
      C.ALet (f, c, expr rest)
  and complex = function
    | A.CImm i -> C.CImm i
    | A.CNeg i -> C.CNeg i
    | A.CBinOp (op, a, b) -> C.CBinOp (op, a, b)
    | A.CApp (f, args) -> C.ApplyClosure (f, args)
    | A.CIf (c, t, e) ->
      let t = expr t in
      C.CIf (c, t, expr e)
    | A.CFun (ps, body) -> closure None ps body
  and closure recursive params body =
    let label = fresh "cc_code" in
    let self = fresh "cc_self" in
    let bound =
      match recursive with
      | None -> params
      | Some f -> f :: params
    in
    let captures = Names.elements (without bound (fv body)) in
    let converted = expr body in
    let converted =
      match recursive with
      | Some f when (not (List.mem f params)) && Names.mem f (fv body) ->
        C.ALet (f, C.CImm (C.ImmVar self), converted)
      | _ -> converted
    in
    let body =
      List.fold_right
        (fun (i, x) rest -> C.ALet (x, C.GetEnv (self, i), rest))
        (List.mapi (fun i x -> i, x) captures)
        converted
    in
    C.MakeClosure
      ({ label; self; params; captures; body }, List.map (fun x -> C.ImmVar x) captures)
  in
  List.map
    (function
      | A.DExpr e -> C.DExpr (expr e)
      | A.DLet (x, [], e) -> C.DLet (x, expr e)
      | A.DLet (x, ps, e) -> C.DLet (x, C.AExpr (closure None ps e))
      | A.DLetRec (x, ps, e) -> C.DLet (x, C.AExpr (closure (Some x) ps e)))
    program
;;
