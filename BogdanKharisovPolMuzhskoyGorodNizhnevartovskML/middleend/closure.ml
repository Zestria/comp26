[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** A closure has explicit environment slots and captured values. Its code may
    only refer to those slots and its parameters. The layout of a heap object
    remains a backend decision. *)
type t =
  { environment : Id.t list
  ; parameter : Id.t
  ; parameters : Id.t list
  ; body : expression
  ; captures : Anf.atom list
  }

and expression = t Anf.term

type error =
  | Binding of Lambda.invariant_error
  | Capture_count of int * int

let error_to_string = function
  | Binding error -> Lambda.invariant_error_to_string error
  | Capture_count (expected, actual) ->
    Printf.sprintf
      "Closure needs %d environment slots but has %d captures"
      expected
      actual
;;

let validate expression =
  let ( let* ) = Result.bind in
  let binding_error error = Binding error in
  let rec function_ declared outer definition =
    let expected = List.length definition.environment in
    let actual = List.length definition.captures in
    if expected <> actual
    then Error (Capture_count (expected, actual))
    else
      let* () =
        Result.map_error binding_error (Anf.Scope.atoms outer definition.captures)
      in
      let* declared, local =
        Result.map_error
          binding_error
          (Anf.Scope.bind_all
             declared
             Id.Set.empty
             (definition.environment @ (definition.parameter :: definition.parameters)))
      in
      Anf.Scope.term ~error:binding_error function_ declared local definition.body
  in
  Anf.Scope.run (fun declared ->
    Anf.Scope.term ~error:binding_error function_ declared Id.Set.empty expression)
;;
