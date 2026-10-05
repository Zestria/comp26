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

