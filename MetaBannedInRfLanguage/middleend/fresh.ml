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
  ; generated : SSet.t
  ; counter : int
  }

let create ~taken = { taken; generated = SSet.empty; counter = 0 }
let create_from_program program = create ~taken:(program_idents program)

let fresh ?(suffix = "__f") t base =
  let rec attempt counter =
    let name = Printf.sprintf "%s%s%d" base suffix counter in
    if SSet.mem name t.taken || SSet.mem name t.generated
    then attempt (counter + 1)
    else name, { t with generated = SSet.add name t.generated; counter }
  in
  attempt (t.counter + 1)
;;

type 'a gen = t -> 'a * t

let return value t = value, t

let ( let* ) computation f t =
  let value, t = computation t in
  f value t
;;

let name ?suffix base t = fresh ?suffix t base

let map_list f items t =
  let reversed, t =
    List.fold_left
      (fun (acc, t) item ->
         let value, t = f item t in
         value :: acc, t)
      ([], t)
      items
  in
  List.rev reversed, t
;;

let run t computation = fst (computation t)
