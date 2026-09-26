[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml

let default_extension =
  { Parser_intf.glued_keywords = false; Parser_intf.operator_declarations = false }
;;

let glued_keywords =
  { Parser_intf.glued_keywords = true; Parser_intf.operator_declarations = false }
;;

let measureTime parser program =
  let start_time = Unix.gettimeofday () in
  let _ = ignore (parser program) in
  let end_time = Unix.gettimeofday () in
  print_float (end_time -. start_time);
  print_string ", "
;;

let bench ?(iterations : int = 20) (programs : string list) =
  let menhir = Menhir_frontend.parse_expr default_extension in
  let rd = Rd_parser.parse_expr default_extension in
  let rd_ext = Rd_parser.parse_expr glued_keywords in
  print_string "menhir = [";
  for _ = 1 to iterations do
    Gc.full_major ();
    List.map (measureTime menhir) programs |> ignore
  done;
  print_string "]\n";
  print_string "rd = [";
  for _ = 1 to iterations do
    Gc.full_major ();
    List.map (measureTime rd) programs |> ignore
  done;
  print_string "]\n";
  print_string "rd_ext = [";
  for _ = 1 to iterations do
    Gc.full_major ();
    List.map (measureTime rd_ext) programs |> ignore
  done;
  print_endline "]"
;;

let main () =
  print_endline "Small expressions";
  bench ~iterations:100 (Generate.generate_expressions 5 400);
  print_endline "Large expressions";
  bench ~iterations:100 (Generate.generate_expressions 30 400)
;;

main ()
