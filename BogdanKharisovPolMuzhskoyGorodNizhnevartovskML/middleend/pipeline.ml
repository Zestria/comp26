[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type error =
  | Source_error of Lower.error
  | Recursive_error of Recursive_check.error
  | Invariant_error of string * Lambda.invariant_error
  | Identifier_error of Id.error

let error_to_string = function
  | Identifier_error error -> Id.error_to_string error
  | Source_error error -> Lower.error_to_string error
  | Recursive_error error -> Recursive_check.error_to_string error
  | Invariant_error (stage, error) ->
    let detail = Lambda.invariant_error_to_string error in
    "Internal invariant failure after " ^ stage ^ ": " ^ detail
;;

let ( let* ) = Result.bind

let source_error = function
  | Lower.Identifier_error error -> Identifier_error error
  | error -> Source_error error
;;

let allocated result = Result.map_error (fun error -> Identifier_error error) result

let source lower check supply source =
  let* lambda, supply = Result.map_error source_error (lower supply source) in
  let* () = Result.map_error (fun e -> Recursive_error e) (check source) in
  let* () =
    Result.map_error (fun e -> Invariant_error ("lowering", e)) (Lambda.validate lambda)
  in
  Ok (lambda, supply)
;;

let normalize supply lambda =
  let* result, supply = allocated (Anf.of_lambda supply lambda) in
  let* () =
    Result.map_error (fun e -> Invariant_error ("ANF", e)) (Anf.validate result)
  in
  Ok (result, supply)
;;

let normalize_program supply program =
  let* lambda, supply = source Lower.program Recursive_check.program supply program in
  normalize supply lambda
;;

let run action = Result.map fst (action Id.initial)
let anf program = run (fun supply -> normalize_program supply program)
