[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Anf
module Names = Set.Make (String)
module Env = Map.Make (String)

let names_imm = function
  | A.ImmInt _ -> Names.empty
  | A.ImmVar x -> Names.singleton x
;;

let names_cond = function
  | A.CondImm i -> names_imm i
  | A.CondCmp (_, a, b) -> Names.union (names_imm a) (names_imm b)
;;

let rec names_expr = function
  | A.AExpr c -> names_complex c
  | A.ALet (x, c, rest) -> Names.add x (Names.union (names_complex c) (names_expr rest))
  | A.ALetRec (f, params, body, rest) ->
    Names.union
      (Names.of_list (f :: params))
      (Names.union (names_expr body) (names_expr rest))

and names_complex = function
  | A.CImm i | A.CNeg i -> names_imm i
  | A.CBinOp (_, a, b) -> Names.union (names_imm a) (names_imm b)
  | A.CApp (f, args) ->
    List.fold_left (fun s i -> Names.union s (names_imm i)) (names_imm f) args
  | A.CIf (c, t, e) ->
    Names.union (names_cond c) (Names.union (names_expr t) (names_expr e))
  | A.CFun (params, body) -> Names.union (Names.of_list params) (names_expr body)
;;

module State = struct
  type state =
    { used : Names.t
    ; definitions : A.decl list
    }

  let return x state = x, state

  let ( let* ) m f state =
    let x, state = m state in
    f x state
  ;;

  let fresh prefix state =
    let rec find n =
      let name = prefix ^ Int.to_string n in
      if Names.mem name state.used then find (n + 1) else name
    in
    let name = find 0 in
    name, { state with used = Names.add name state.used }
  ;;

  let emit definition state =
    (), { state with definitions = definition :: state.definitions }
  ;;

  let rec map_list f = function
    | [] -> return []
    | x :: xs ->
      let* x = f x in
      let* xs = map_list f xs in
      return (x :: xs)
  ;;
end

open State

let hide env names = List.fold_left (fun env x -> Env.remove x env) env names

let immediate env = function
  | A.ImmInt _ as i -> i
  | A.ImmVar x -> A.ImmVar (Option.value (Env.find_opt x env) ~default:x)
;;

let condition env = function
  | A.CondImm i -> A.CondImm (immediate env i)
  | A.CondCmp (op, a, b) -> A.CondCmp (op, immediate env a, immediate env b)
;;

let rec expr env = function
  | A.AExpr c ->
    let* c = complex env c in
    return (A.AExpr c)
  | A.ALet (f, A.CFun (params, body), rest) ->
    let* name = fresh "lifted_fun" in
    let* body = expr (hide env params) body in
    let* () = emit (A.DLet (name, params, body)) in
    expr (Env.add f name env) rest
  | A.ALet (x, c, rest) ->
    let* c = complex env c in
    let* rest = expr (hide env [ x ]) rest in
    return (A.ALet (x, c, rest))
  | A.ALetRec (f, params, body, rest) ->
    let* name = fresh "lifted_fun" in
    let env_f = Env.add f name env in
    let* body = expr (hide env_f params) body in
    let* () = emit (A.DLetRec (name, params, body)) in
    expr env_f rest

and complex env = function
  | A.CImm i -> return (A.CImm (immediate env i))
  | A.CNeg i -> return (A.CNeg (immediate env i))
  | A.CBinOp (op, a, b) -> return (A.CBinOp (op, immediate env a, immediate env b))
  | A.CApp (f, args) -> return (A.CApp (immediate env f, List.map (immediate env) args))
  | A.CIf (c, t, e) ->
    let* t = expr env t in
    let* e = expr env e in
    return (A.CIf (condition env c, t, e))
  | A.CFun (params, body) ->
    let* name = fresh "lifted_fun" in
    let* body = expr (hide env params) body in
    let* () = emit (A.DLet (name, params, body)) in
    return (A.CImm (A.ImmVar name))
;;

let of_program program =
  let names_decl = function
    | A.DExpr e -> names_expr e
    | A.DLet (x, params, body) | A.DLetRec (x, params, body) ->
      Names.union (Names.of_list (x :: params)) (names_expr body)
  in
  let used =
    List.fold_left (fun s d -> Names.union s (names_decl d)) Names.empty program
  in
  let declarations, state =
    map_list
      (function
        | A.DExpr e ->
          let* e = expr Env.empty e in
          let* name = fresh "lifted_main" in
          return (A.DLet (name, [], e))
        | A.DLet (x, params, body) ->
          let* body = expr Env.empty body in
          return (A.DLet (x, params, body))
        | A.DLetRec (f, params, body) ->
          let* body = expr Env.empty body in
          return (A.DLetRec (f, params, body)))
      program
      { used; definitions = [] }
  in
  (* Only closed function values move before the original declarations;
     computations and partial applications stay at their original sites. *)
  List.rev state.definitions @ declarations
;;
