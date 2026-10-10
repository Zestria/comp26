[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module SSet = Set.Make (String)

let rec idents_imm acc = function
  | ImmVar name -> SSet.add name acc
  | ImmInt _ | ImmBool _ | ImmUnit -> acc
;;

let rec idents_cexpr acc = function
  | CImm imm -> idents_imm acc imm
  | CApp (fn, args) -> List.fold_left idents_imm (idents_imm acc fn) args
  | CTuple (first, second, rest) ->
    List.fold_left idents_imm (idents_imm (idents_imm acc first) second) rest
  | CIte (cond, then_branch, else_branch) ->
    idents_aexpr (idents_aexpr (idents_imm acc cond) then_branch) else_branch
  | CFun (params, body) ->
    idents_aexpr (List.fold_left (fun acc p -> SSet.add p acc) acc params) body
  | CMatchFailure -> acc

and idents_aexpr acc = function
  | ACExpr cexpr -> idents_cexpr acc cexpr
  | ALet (name, value, body) -> idents_aexpr (idents_cexpr (SSet.add name acc) value) body
  | ALetTuple (names, subject, body) ->
    let acc =
      List.fold_left
        (fun acc name ->
           match name with
           | Some name -> SSet.add name acc
           | None -> acc)
        (idents_imm acc subject)
        names
    in
    idents_aexpr acc body
  | ALetRec (bindings, body) ->
    let acc =
      List.fold_left
        (fun acc (name, value) -> idents_cexpr (SSet.add name acc) value)
        acc
        bindings
    in
    idents_aexpr acc body
;;

let idents_item acc = function
  | AValue (name, body) -> idents_aexpr (SSet.add name acc) body
  | ARec bindings ->
    List.fold_left
      (fun acc (name, value) -> idents_cexpr (SSet.add name acc) value)
      acc
      bindings
  | AEval body -> idents_aexpr acc body
;;

let program_idents program = List.fold_left idents_item SSet.empty program

type t =
  { taken : SSet.t
  ; mutable generated : SSet.t
  ; mutable counter : int
  }

let create ~taken = { taken; generated = SSet.empty; counter = 0 }
let create_from_program program = create ~taken:(program_idents program)

let fresh ?(suffix = "__f") t base =
  let rec attempt () =
    t.counter <- t.counter + 1;
    let name = Printf.sprintf "%s%s%d" base suffix t.counter in
    if SSet.mem name t.taken || SSet.mem name t.generated
    then attempt ()
    else (
      t.generated <- SSet.add name t.generated;
      name)
  in
  attempt ()
;;
