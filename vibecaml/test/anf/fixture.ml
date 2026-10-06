[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml

let fail message =
  prerr_endline message;
  exit 1
;;

let () =
  let raw, stats, filename =
    match Array.to_list Sys.argv with
    | [ _; filename ] -> false, false, filename
    | [ _; "--raw"; filename ] -> true, false, filename
    | [ _; "--stats"; filename ] -> false, true, filename
    | _ -> fail "fixture [--raw|--stats] input.src"
  in
  let channel = open_in filename in
  let source =
    Fun.protect
      ~finally:(fun () -> close_in channel)
      (fun () -> really_input_string channel (in_channel_length channel))
  in
  (* Общий Fresh ещё не смержен. У тестового callback свой счётчик;
     каждый запрос имени продвигает его один раз. *)
  let counter = ref 0 in
  let fresh base =
    incr counter;
    Printf.sprintf "%s__%d" base !counter
  in
  match Rd_parser.parse_structure Config.default_extensions source with
  | Error message -> fail message
  | Ok source ->
    (match Anf_conv.convert ~fresh source with
     | Error message -> fail message
     | Ok program ->
       let smaller = Anf_simplify.simplify program in
       if stats
       then
         Printf.printf "let: %d -> %d\n" (Anf.count_lets program) (Anf.count_lets smaller)
       else print_endline (Anf.string_of_program (if raw then program else smaller)))
;;
