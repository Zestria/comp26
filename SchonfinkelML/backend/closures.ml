[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Anf = SchonfinkelML_middleend.Anf

(* Number of leading CAbstr — the arity of a lifted function. *)
let rec count_abstrs_aexpr = function
  | Anf.ACExpr (Anf.CAbstr (_, body)) -> 1 + count_abstrs_aexpr body
  | _ -> 0

let rec collect_arities acc = function
  | Anf.ALet (_, name, rhs, rest) ->
    let acc =
      match rhs with
      | Anf.ACExpr (Anf.CAbstr _) -> (name, count_abstrs_aexpr rhs) :: acc
      | _ -> acc
    in
    collect_arities acc rest
  | Anf.ACExpr _ -> acc

(* Turn under-applied CApp into CMkClosure, unknown-head CApp into CApplyClosure. *)
let rec lower_cexpr arities = function
  | Anf.CApp (Anf.ImmId f, a, rest) ->
      let args = a :: rest in
      let n = List.length args in
      (match List.assoc_opt f arities with
       | Some ar when ar = n -> Anf.CApp (Anf.ImmId f, a, rest)
       | Some ar when ar > n -> Anf.CMkClosure (f, args)
       | Some ar ->
           failwith
             (Printf.sprintf "over-application of %s (arity %d, applied %d)" f ar n)
       | None -> Anf.CApplyClosure (Anf.ImmId f, args))
  | Anf.CApp (f, a, rest) -> Anf.CApplyClosure (f, a :: rest)
  | Anf.CIfExpr (c, t, e) ->
      Anf.CIfExpr (c, lower_aexpr arities t, lower_aexpr arities e)
  | Anf.CAbstr (p, body) -> Anf.CAbstr (p, lower_aexpr arities body)
  | (Anf.CBinop _ | Anf.CImmExpr _ | Anf.CMkClosure _ | Anf.CApplyClosure _) as c -> c

and lower_aexpr arities = function
  | Anf.ALet (flag, name, rhs, body) ->
      Anf.ALet (flag, name, lower_aexpr arities rhs, lower_aexpr arities body)
  | Anf.ACExpr c -> Anf.ACExpr (lower_cexpr arities c)

let lower a =
  let arities = collect_arities [] a in
  lower_aexpr arities a
