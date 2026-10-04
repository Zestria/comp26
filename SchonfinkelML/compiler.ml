[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
open SchonfinkelML_frontend.Config

let write_output path text =
  let write oc =
    output_string oc text;
    output_char oc '\n'
  in
  if path = "-"
  then write stdout
  else (
    let oc = open_out path in
    Fun.protect ~finally:(fun () -> close_out oc) (fun () -> write oc))
;;

let parser_ops cfg =
  match cfg.parser_settings with
  | None -> Top_down.Operators.default
  | Some settings ->
    (match Top_down.Operators.of_string settings with
     | Ok ops -> ops
     | Error msg ->
       Printf.eprintf "invalid --parser-settings: %s\n" msg;
       exit 2)
;;

let parse cfg src =
  match cfg.parser with
  | Bottom_up ->
    (match Bottom_up.Parser.parse_expr src with
     | Ok expr -> expr
     | Error () ->
       Printf.eprintf "bottom-up parser: syntax error\n";
       exit 1)
  | Top_down ->
    (match Top_down.Parser.parse_expr ~ops:(parser_ops cfg) src with
     | Ok expr -> expr
     | Error err ->
       Printf.eprintf "%s\n" (Top_down.Parser.string_of_error err);
       exit 1)
;;

let lower cfg expr =
  match cfg.stop_after with
  | Some Parsing -> string_of_expr expr
  | Some Anf -> Middleend.Anf.string_of_aexpr (Middleend.Anf.create_anf expr)
  | None -> string_of_expr (Middleend.Closure_conversion.closure_convert expr)
;;

let run input cfg =
  let src = In_channel.with_open_bin input In_channel.input_all in
  let expr = parse cfg src in
  write_output cfg.output (lower cfg expr)
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
