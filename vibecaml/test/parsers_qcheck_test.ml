[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Свойство [parse (pp ast) = ast] для обоих парсеров на одних и тех же
   деревьях: расхождение парсеров всплывает сразу. *)

open Vibecaml

let parsers =
  [ "menhir", (module Menhir_frontend : Parser_intf.S)
  ; "rd", (module Rd_parser : Parser_intf.S)
  ]
;;

let round_trip print parse tree =
  let printed = print tree in
  match parse printed with
  | Ok parsed when parsed = tree -> true
  | Ok parsed -> QCheck.Test.fail_reportf "%s\nразобрано как\n%s" printed (print parsed)
  | Error message -> QCheck.Test.fail_reportf "%s\n%s" message printed
;;

let () =
  let identifiers = Generate.core_identifiers in
  Generate.check
    (List.concat_map
       (fun (name, (module P : Parser_intf.S)) ->
          [ QCheck.Test.make
              ~count:1000
              ~name:(name ^ ": выражение разбирается в то же дерево")
              (QCheck.make
                 ~print:Pprint.string_of_expression
                 ~shrink:Generate.shrink_expression
                 (Generate.expression identifiers))
              (round_trip Pprint.string_of_expression P.parse_expr)
          ; QCheck.Test.make
              ~count:1000
              ~name:(name ^ ": программа разбирается в то же дерево")
              (QCheck.make
                 ~print:Pprint.string_of_structure
                 ~shrink:Generate.shrink_structure
                 (Generate.structure identifiers))
              (round_trip Pprint.string_of_structure P.parse_structure)
          ])
       parsers)
;;
