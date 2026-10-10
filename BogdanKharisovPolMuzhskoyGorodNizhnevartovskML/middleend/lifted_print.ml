[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Format

let program formatter (program : Lifted.program) =
  let closure_names names (closure : Lifted.closure) =
    let names = Anf_print.Names.add names closure.code in
    List.fold_left Anf_print.Names.atom names closure.captures
  in
  let term_names = Anf_print.names ~function_:closure_names in
  let definition_names names (definition : Lifted.function_definition) =
    let names = Anf_print.Names.add names definition.code in
    let names = Anf_print.Names.add_all names definition.environment in
    let names =
      Anf_print.Names.add_all names (definition.parameter :: definition.parameters)
    in
    term_names names definition.body
  in
  let names = List.fold_left definition_names Anf_print.Names.empty program.functions in
  let names = term_names names program.body in
  let printers =
    Anf_print.make
      names
      (fun identifier atom _term formatter (closure : Lifted.closure) ->
         fprintf
           formatter
           "@[<hov>closure %a [%a]@]"
           identifier
           closure.code
           (Anf_print.separated "," atom)
           closure.captures)
  in
  List.iter
    (fun (definition : Lifted.function_definition) ->
       fprintf
         formatter
         "@[<v 2>@[<hov 2>code %a [%a] %a =@]@,%a@]@.@."
         printers.identifier
         definition.code
         (Anf_print.separated "," printers.identifier)
         definition.environment
         (Anf_print.separated "" printers.identifier)
         (definition.parameter :: definition.parameters)
         printers.term
         definition.body)
    program.functions;
  printers.term formatter program.body
;;
