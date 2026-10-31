[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Names = Map.Make (String)

type interval =
  { name : string
  ; start : int
  ; finish : int
  }

type assignment =
  | Register of register
  | Spill of int

let registers = [ S0; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11 ]

type analysis =
  { position : int
  ; definitions : int Names.t
  ; uses : int Names.t
  }

let define state = function
  | PVar name ->
    { state with definitions = Names.add name state.position state.definitions }
  | PUnit | PUnderscore -> state
;;

let use state = function
  | ImmId name -> { state with uses = Names.add name state.position state.uses }
  | ImmInt _ | ImmBool _ | ImmUnit -> state
;;

let advance state = { state with position = state.position + 1 }

let rec analyze_expression state = function
  | ACExpr computation -> analyze_computation state computation
  | ALet (pattern, computation, body) ->
    let state = analyze_computation state computation in
    let state = advance (define state pattern) in
    analyze_expression state body

and analyze_computation state = function
  | CImm value | CUnOp (_, value) -> advance (use state value)
  | CBinOp (_, left, right) -> advance (use (use state left) right)
  | CApp (function_, arguments) ->
    advance (List.fold_left use (use state function_) arguments)
  | CIf (condition, if_true, if_false) ->
    let state = advance (use state condition) in
    let state = analyze_expression state if_true in
    advance (analyze_expression state if_false)
;;

let compare_interval left right =
  let order = Int.compare left.start right.start in
  if order = 0 then String.compare left.name right.name else order
;;

let intervals parameters bodies =
  let initial = { position = 0; definitions = Names.empty; uses = Names.empty } in
  let initial = List.fold_left define initial parameters |> advance in
  let state = List.fold_left analyze_expression initial bodies in
  Names.fold
    (fun name start intervals ->
      match Names.find_opt name state.uses with
      | Some finish -> { name; start; finish } :: intervals
      | None -> intervals)
    state.definitions []
  |> List.sort compare_interval
;;

let allocate ~registers intervals =
  let step (active, assigned, next_spill) current =
    let active = List.filter (fun (interval, _) -> interval.finish >= current.start) active in
    let free =
      List.find_opt
        (fun register -> not (List.exists (fun (_, used) -> used = register) active))
        registers
    in
    match free with
    | Some register ->
      ( (current, register) :: active
      , Names.add current.name (Register register) assigned
      , next_spill )
    | None ->
      let furthest =
        List.fold_left
          (fun candidate ((interval, _) as entry) ->
            match candidate with
            | None -> Some entry
            | Some (previous, _) when interval.finish > previous.finish -> Some entry
            | Some _ -> candidate)
          None active
      in
      (match furthest with
       | Some (victim, register) when victim.finish > current.finish ->
         let active = List.filter (fun (interval, _) -> interval.name <> victim.name) active in
         ( (current, register) :: active
         , assigned
           |> Names.add victim.name (Spill next_spill)
           |> Names.add current.name (Register register)
         , next_spill + 1 )
       | _ -> active, Names.add current.name (Spill next_spill) assigned, next_spill + 1)
  in
  let _, assigned, _ =
    List.fold_left step ([], Names.empty, 0) (List.sort compare_interval intervals)
  in
  Names.bindings assigned
;;
