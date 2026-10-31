[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Bkml_middleend
open Anf

type graph = Id.Set.t Id.Map.t

let add_atom live = function
  | Var id -> Id.Set.add id live
  | Constant _ | Primitive _ -> live
;;

let add_atoms live atoms = List.fold_left add_atom live atoms

let rec live_in expression out =
  match expression with
  | Return value -> live_in_computation value out
  | Let (id, value, body) ->
    let following = Id.Set.remove id (live_in body out) in
    live_in_computation value following
  | Let_rec (id, value, body) ->
    let following = live_in body out in
    Id.Set.remove id (live_in value (Id.Set.add id following))
  | Sequence (value, body) -> live_in_computation value (live_in body out)

and live_in_computation value out =
  match value with
  | Atom atom | Field (atom, _) -> add_atom out atom
  | Tuple (first, second, rest) -> add_atoms out (first :: second :: rest)
  | Function closure -> add_atoms out closure.Lifted.captures
  | Apply (callee, first, rest) ->
    let live = add_atoms out (first :: rest) in
    (match callee with
     | Lifted.Direct _ -> live
     | Indirect atom -> add_atom live atom)
  | If (condition, yes, no) ->
    let branches = Id.Set.union (live_in yes out) (live_in no out) in
    add_atom branches condition
;;

let ensure id graph =
  match Id.Map.find_opt id graph with
  | Some _ -> graph
  | None -> Id.Map.add id Id.Set.empty graph
;;

let add_neighbour id neighbour =
  Id.Map.update id (function
    | None -> Some (Id.Set.singleton neighbour)
    | Some neighbours -> Some (Id.Set.add neighbour neighbours))
;;

let add_edge left right graph =
  if Id.equal left right
  then ensure left graph
  else add_neighbour right left (add_neighbour left right graph)
;;

let interfere_with id neighbours graph =
  Id.Set.fold
    (fun neighbour graph -> add_edge id neighbour graph)
    neighbours
    (ensure id graph)
;;

type analysis =
  { input : Id.Set.t
  ; anywhere : Id.Set.t
  ; graph : graph
  }

let analysis input inside graph = { input; anywhere = Id.Set.union input inside; graph }

let rec analyze_term graph out expression =
  match expression with
  | Return value -> analyze_computation graph out value
  | Let (id, value, body) ->
    let body = analyze_term graph out body in
    let following = Id.Set.remove id body.input in
    let value = analyze_computation body.graph following value in
    let graph = interfere_with id following value.graph in
    analysis value.input (Id.Set.union value.anywhere body.anywhere) graph
  | Let_rec (id, value, body) ->
    let body = analyze_term graph out body in
    let value = analyze_term body.graph (Id.Set.add id body.input) value in
    let neighbours = Id.Set.remove id (Id.Set.union value.anywhere body.input) in
    let graph = interfere_with id neighbours value.graph in
    analysis
      (Id.Set.remove id value.input)
      (Id.Set.union value.anywhere body.anywhere)
      graph
  | Sequence (value, body) ->
    let body = analyze_term graph out body in
    let value = analyze_computation body.graph body.input value in
    analysis value.input (Id.Set.union value.anywhere body.anywhere) value.graph

and analyze_computation graph out value =
  match value with
  | Atom atom | Field (atom, _) -> analysis (add_atom out atom) Id.Set.empty graph
  | Tuple (first, second, rest) ->
    analysis (add_atoms out (first :: second :: rest)) Id.Set.empty graph
  | Function closure ->
    analysis (add_atoms out closure.Lifted.captures) Id.Set.empty graph
  | Apply (callee, first, rest) ->
    let live = add_atoms out (first :: rest) in
    let input =
      match callee with
      | Lifted.Direct _ -> live
      | Indirect atom -> add_atom live atom
    in
    analysis input Id.Set.empty graph
  | If (condition, yes, no) ->
    let yes = analyze_term graph out yes in
    let no = analyze_term yes.graph out no in
    let branches = Id.Set.union yes.input no.input in
    analysis
      (add_atom branches condition)
      (Id.Set.union yes.anywhere no.anywhere)
      no.graph
;;

let entry_graph entry =
  let entries = Id.Set.of_list entry in
  List.fold_left
    (fun graph id -> Id.Map.add id (Id.Set.remove id entries) graph)
    Id.Map.empty
    entry
;;

let interference ~entry expression =
  let result = analyze_term (entry_graph entry) Id.Set.empty expression in
  result.graph
;;
