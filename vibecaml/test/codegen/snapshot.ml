[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml

let unwrap = function
  | Ok value -> value
  | Error message ->
    prerr_endline message;
    exit 1
;;

let () =
  Fresh.reset ();
  let source = In_channel.with_open_bin Sys.argv.(1) In_channel.input_all in
  let ast = unwrap (Rd_parser.parse_structure Config.default_extensions source) in
  let renamed = unwrap (Rename.rename ast) in
  let closed = unwrap (Closure_conv.convert renamed) in
  let lifted = unwrap (Lambda_lift.lift closed) in
  let anf = unwrap (Anf_conv.convert lifted) |> Anf_simplify.simplify in
  let program = unwrap (Codegen.generate anf) |> Riscv.peephole in
  Format.printf "%a" Riscv.pp_program program
;;
