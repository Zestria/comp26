[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let program formatter expression =
  let rec closure_names names (closure : Closure.t) =
    let names = List.fold_left Anf_print.Names.atom names closure.captures in
    let names = Anf_print.Names.add_all names closure.environment in
    let names = Anf_print.Names.add_all names (closure.parameter :: closure.parameters) in
    Anf_print.names ~function_:closure_names names closure.body
  in
  let names = Anf_print.names ~function_:closure_names Anf_print.Names.empty expression in
  let printers =
    Anf_print.make names (fun identifier atom term formatter (closure : Closure.t) ->
      Format.fprintf
        formatter
        "@[<hov 2>closure [%a] (fun [%a] %a ->@ %a)@]"
        (Anf_print.separated "," atom)
        closure.captures
        (Anf_print.separated "," identifier)
        closure.environment
        (Anf_print.separated "" identifier)
        (closure.parameter :: closure.parameters)
        term
        closure.body)
  in
  printers.term formatter expression
;;
