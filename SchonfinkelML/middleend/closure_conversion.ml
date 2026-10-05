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
