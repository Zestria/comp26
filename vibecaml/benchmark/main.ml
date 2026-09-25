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

let bench ?(iterations : int = 20) (programs : string list) =
  print_endline "Starting warmup";
  for _ = 1 to 5 do
    Rd_parser.parse_expr default_extension (List.hd programs) |> ignore;
    Rd_parser.parse_expr glued_keywords (List.hd programs) |> ignore;
    Menhir_frontend.parse_expr default_extension (List.hd programs) |> ignore
  done;
  print_endline "Warmup done";
  print_endline "Starting benchmarking";
  print_string "Menhir = [";
  Gc.full_major ();
  for i = 1 to iterations do
    let startTime = Unix.gettimeofday () in
    List.map (Menhir_frontend.parse_expr default_extension) programs |> ignore;
    let endTime = Unix.gettimeofday () in
    print_float (endTime -. startTime);
    print_string ", "
  done;
  print_endline "]";
  print_string "RD = [";
  Gc.full_major ();
  for i = 1 to iterations do
    let startTime = Unix.gettimeofday () in
    List.map (Rd_parser.parse_expr default_extension) programs |> ignore;
    let endTime = Unix.gettimeofday () in
    print_float (endTime -. startTime);
    print_string ", "
  done;
  print_endline "]";
  print_string "RD_Extension = [";
  Gc.full_major ();
  for i = 1 to iterations do
    let startTime = Unix.gettimeofday () in
    List.map (Rd_parser.parse_expr glued_keywords) programs |> ignore;
    let endTime = Unix.gettimeofday () in
    print_float (endTime -. startTime);
    print_string ", "
  done;
  print_endline "]";
  print_endline "Done"
;;

let main () =
  print_endline "Small expressions";
  bench (Generate.generate_expressions 1 10);
  print_endline "Medium expressions";
  bench (Generate.generate_expressions 7 10);
  print_endline "Large expressions";
  bench (Generate.generate_expressions 10 10)
;;

main ()
