[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let print_string = Printf.sprintf "%S"

let require_valid create print_error input =
  match create input with
  | Ok value -> value
  | Error error ->
    QCheck.Test.fail_reportf
      "Expected valid test input %S; got %s"
      input
      (print_error error)
;;

let identifier =
  require_valid Bkml_frontend.Name.of_identifier Bkml_frontend.Name.error_to_string
;;

let operator =
  require_valid Bkml_frontend.Name.of_operator Bkml_frontend.Name.error_to_string
;;

let integer_literal =
  require_valid
    Bkml_frontend.Integer_literal.of_string
    Bkml_frontend.Integer_literal.error_to_string
;;

let equal ~print ~expected actual =
  actual = expected
  || QCheck.Test.fail_reportf "Expected: %s\nActual:   %s" (print expected) (print actual)
;;

let strings alphabet =
  let generator =
    QCheck.Gen.(
      string_size
        ~gen:(oneof_list (List.of_seq (String.to_seq alphabet)))
        (int_range 0 30))
  in
  QCheck.make ~print:print_string ~shrink:QCheck.Shrink.string generator
  |> QCheck.add_shrink_invariant (String.for_all (String.contains alphabet))
;;

let property name arbitrary check = QCheck.Test.make ~name ~count:2000 arbitrary check
