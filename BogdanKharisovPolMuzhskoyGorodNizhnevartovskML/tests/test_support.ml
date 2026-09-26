[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let print_string = Printf.sprintf "%S"

let equal ~print ~expected actual =
  if actual = expected
  then true
  else
    QCheck.Test.fail_reportf "Expected: %s\nActual:   %s" (print expected) (print actual)
;;

let strings alphabet =
  QCheck.Gen.(
    string_size ~gen:(oneof_list (List.of_seq (String.to_seq alphabet))) (int_range 0 30))
;;

let property name generator check =
  QCheck.Test.make
    ~name
    ~count:2000
    (QCheck.make ~print:print_string ~shrink:QCheck.Shrink.string generator)
    check
;;
