[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let lift supply expression =
  let ( let* ) = Result.bind in
  let rec term state expression =
    Anf.map_accum_result
      ~callee:(fun value ~arity:_ -> Lifted.Indirect value)
      ~atom:Fun.id
      ~function_
      state
      expression
  and function_ state (closure : Closure.t) =
    let* (supply, definitions), body = term state closure.body in
    let* code, supply = Id.fresh supply "code" in
    let { Closure.environment; parameter; parameters; captures; body = _ } = closure in
    let definition : Lifted.function_definition =
      { code; environment; parameter; parameters; body }
    in
    let closure : Lifted.closure = { code; captures } in
    Ok ((supply, definition :: definitions), closure)
  in
  let* (supply, definitions), body = term (supply, []) expression in
  Ok ({ Lifted.functions = List.rev definitions; body }, supply)
;;
