[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type analyzed_function =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : analyzed_function Anf.term
  ; free : Id.Set.t
  }

let add_atom free = function
  | Anf.Var id -> Id.Set.add id free
  | Constant _ | Primitive _ -> free
;;

(* Compute each subtree's free variables once, from its children. Retaining
   the result on function values avoids rescanning all nested bodies when
   their closure environments are built. The remaining term forms stay ANF. *)
let rec analyze = function
  | Anf.Return value ->
    let value, free = analyze_computation value in
    Anf.Return value, free
  | Let (id, value, body) ->
    let value, value_free = analyze_computation value in
    let body, body_free = analyze body in
    Anf.Let (id, value, body), Id.Set.union value_free (Id.Set.remove id body_free)
  | Let_rec (id, value, body) ->
    let value, value_free = analyze value in
    let body, body_free = analyze body in
    Anf.Let_rec (id, value, body), Id.Set.remove id (Id.Set.union value_free body_free)
  | Sequence (first, second) ->
    let first, first_free = analyze_computation first in
    let second, second_free = analyze second in
    Anf.Sequence (first, second), Id.Set.union first_free second_free

and analyze_computation = function
  | Anf.Atom value -> Anf.Atom value, add_atom Id.Set.empty value
  | Tuple (a, b, rest) ->
    Anf.Tuple (a, b, rest), List.fold_left add_atom Id.Set.empty (a :: b :: rest)
  | Field (value, index) -> Anf.Field (value, index), add_atom Id.Set.empty value
  | Apply (f, a, rest) ->
    Anf.Apply (f, a, rest), List.fold_left add_atom Id.Set.empty (f :: a :: rest)
  | If (condition, yes, no) ->
    let yes, yes_free = analyze yes in
    let no, no_free = analyze no in
    Anf.If (condition, yes, no), add_atom (Id.Set.union yes_free no_free) condition
  | Function ({ parameter; parameters; body } : Anf.function_definition) ->
    let body, body_free = analyze body in
    let free =
      List.fold_left
        (fun free parameter -> Id.Set.remove parameter free)
        body_free
        (parameter :: parameters)
    in
    Anf.Function { parameter; parameters; body; free }, free
;;

let rename names = function
  | Anf.Var id -> Anf.Var (Option.value (Id.Map.find_opt id names) ~default:id)
  | (Constant _ | Primitive _) as atom -> atom
;;

let convert supply expression =
  let ( let* ) = Result.bind in
  let rec close names supply expression =
    Anf.map_accum_result
      ~atom:(rename names)
      ~function_:(function_ names)
      supply
      expression
  and function_ names supply ({ parameter; parameters; body; free } : analyzed_function) =
    let free = Id.Set.elements free in
    let* supply, local, slots =
      List.fold_left
        (fun state id ->
           let* supply, local, slots = state in
           let* slot, supply = Id.rename supply id in
           Ok (supply, Id.Map.add id slot local, slot :: slots))
        (Ok (supply, Id.Map.empty, []))
        free
    in
    let* supply, body = close local supply body in
    let closure : Closure.t =
      { environment = List.rev slots
      ; parameter
      ; parameters
      ; body
      ; captures = List.map (fun id -> rename names (Anf.Var id)) free
      }
    in
    Ok (supply, closure)
  in
  let expression, _free = analyze expression in
  let* supply, expression = close Id.Map.empty supply expression in
  Ok (expression, supply)
;;
