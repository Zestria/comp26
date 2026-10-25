[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

[@@@zanuda "-mutability_check"] (* Mutability is required for Config lib *)

open SchonfinkelML_frontend.Common
open SchonfinkelML_frontend.Config
open SchonfinkelML_middleend.Anf
open SchonfinkelML_middleend.Closure_conversion
open SchonfinkelML_middleend.Lambda_lifting
open SchonfinkelML_backend.Emitter
module BackendCommon = SchonfinkelML_backend.Common
module Writer = SchonfinkelML_backend.Writer
module Middleend = SchonfinkelML_middleend

let write_output path =
  let fd = if path = "-" then stdout else open_out path in
  function
  | Ok rtl ->
    Fun.protect ~finally:(fun () -> close_out fd) (fun () -> Writer.write fd rtl)
  | Error e -> Format.printf "Error: %a\n" BackendCommon.pp_error e
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
  | None ->
    let lifted =
      Middleend.Lambda_lifting.lift (Middleend.Closure_conversion.closure_convert expr)
    in
    string_of_expr lifted
;;

let run input cfg =
  let src = In_channel.with_open_bin input In_channel.input_all in
  let expr = parse cfg src |> closure_convert |> lift in
  let anf = create_anf expr in
  let rtl = get_rtl anf in
  write_output cfg.output rtl
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
