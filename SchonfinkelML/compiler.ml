[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Config

let run input cfg =
  Printf.printf "parser : %s\n" (string_of_parser_kind cfg.parser);
  Printf.printf "output : %s\n" cfg.output;
  Printf.printf "input : %s\n" input
;;

let die fmt =
  Printf.eprintf "%s\n" (Printf.sprintf fmt);
  Arg.usage (make_specs (default_config ())) usage_msg;
  exit 2
;;

let () =
  let cfg = default_config () in
  let specs = make_specs cfg in
  let temp_input = ref None in
  let anon s =
    match !temp_input with
    | None -> temp_input := Some s
    | Some _ -> die "error: too many positional arguments (only one input is allowed)"
  in
  Arg.parse specs anon usage_msg;
  let input =
    match !temp_input with
    | Some s -> s
    | None -> die "error: missing required <input-file>"
  in
  run input cfg
;;
