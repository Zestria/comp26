[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Config

let write_output path expr =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () ->
      output_string oc (Frontend.Common.string_of_expr expr);
      output_char oc '\n')
;;

let run input cfg =
  let src = In_channel.with_open_bin input In_channel.input_all in
  match cfg.parser with
  | Bottom_up ->
    (match Bottom_up.Parser.parse_expr src with
     | Ok expr -> write_output cfg.output expr
     | Error () ->
       Printf.eprintf "bottom-up parser: syntax error in %s\n" input;
       exit 1)
  | Top_down ->
    let ops =
      match cfg.parser_settings with
      | None -> Top_down.Operators.default
      | Some settings ->
        (match Top_down.Operators.of_string settings with
         | Ok ops -> ops
         | Error msg ->
           Printf.eprintf "invalid --parser-settings: %s\n" msg;
           exit 2)
    in
    (match Top_down.Parser.parse_expr ~ops src with
     | Ok expr -> write_output cfg.output expr
     | Error err ->
       Printf.eprintf "%s\n" (Top_down.Parser.string_of_error err);
       exit 1)
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
