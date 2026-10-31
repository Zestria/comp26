[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Middleend

type location =
  | Reg of Riscv.reg
  | Slot of int

type t =
  { locations : location Id.Map.t
  ; saved : Riscv.reg list
  ; slots : int
  }

let definition_order expression =
  List.rev
    (Anf.fold
       ~atom:(fun order _ -> order)
       ~callee:(fun order _ ~arity:_ -> order)
       ~binder:(fun order id -> id :: order)
       ~function_:(fun order _ -> order)
       []
       expression)
;;

let neighbour_locations id graph locations =
  match Id.Map.find_opt id graph with
  | None -> []
  | Some neighbours ->
    Id.Set.fold
      (fun neighbour found ->
         match Id.Map.find_opt neighbour locations with
         | None -> found
         | Some location -> location :: found)
      neighbours
      []
;;

let location_indices locations =
  List.fold_left
    (fun (registers, slots) location ->
       match location with
       | Reg (Riscv.S index) -> index :: registers, slots
       | Reg _ -> registers, slots
       | Slot index -> registers, index :: slots)
    ([], [])
    locations
;;

let rec free_register index used =
  if index > 11
  then None
  else if List.mem index used
  then free_register (index + 1) used
  else Some (Riscv.S index)
;;

let rec free_slot index used =
  if List.mem index used then free_slot (index + 1) used else index
;;

let colour id graph locations =
  let registers, slots = location_indices (neighbour_locations id graph locations) in
  let location =
    match free_register 1 registers with
    | Some register -> Reg register
    | None -> Slot (free_slot 0 slots)
  in
  Id.Map.add id location locations
;;

let allocate ~entry expression =
  let graph = Liveness.interference ~entry expression in
  let order = entry @ definition_order expression in
  let locations =
    List.fold_left (fun locations id -> colour id graph locations) Id.Map.empty order
  in
  let registers, slots =
    Id.Map.fold
      (fun _ location (registers, slots) ->
         match location with
         | Reg (Riscv.S index) -> index :: registers, slots
         | Reg _ -> registers, slots
         | Slot index -> registers, index :: slots)
      locations
      ([], [])
  in
  let saved =
    List.map (fun index -> Riscv.S index) (List.sort_uniq Int.compare registers)
  in
  let slots = List.length (List.sort_uniq Int.compare slots) in
  { locations; saved; slots }
;;
