[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Kormilcyn_khmelev_yakovlev_lib

type parser =
  { name : string
  ; parse : string -> Ast.expr
  }

type benchmark_case =
  { family : string
  ; size : int
  ; source : string
  }

type measurement =
  { iterations : int
  ; median_seconds : float
  ; minimum_seconds : float
  }

let menhir = { name = "menhir"; parse = Main.parse_with_menhir }

let combinator =
  let parse source =
    match Main.parse_with_comb source with
    | Ok ast -> ast
    | Error error -> failwith (Format.asprintf "%a" Parser_comb.pp_error error)
  in
  { name = "combinator"; parse }
;;

let arithmetic terms =
  let buffer = Buffer.create (terms * 8) in
  Buffer.add_string buffer "1";
  for i = 2 to terms do
    Buffer.add_string buffer (if i mod 2 = 0 then " + " else " * ");
    Buffer.add_string buffer (string_of_int ((i mod 97) + 1))
  done;
  Buffer.contents buffer
;;

let application arguments =
  let buffer = Buffer.create (arguments * 2) in
  Buffer.add_string buffer "f";
  for _ = 1 to arguments do
    Buffer.add_string buffer " x"
  done;
  Buffer.contents buffer
;;

let nested_lets depth =
  let buffer = Buffer.create (depth * 18) in
  for i = 1 to depth do
    Printf.bprintf buffer "let x%d = 1 in " i
  done;
  Buffer.add_string buffer "0";
  Buffer.contents buffer
;;

let sizes = [ 100; 1_000; 10_000 ]

let cases =
  List.concat_map
    (fun (family, make_source) ->
      List.map (fun size -> { family; size; source = make_source size }) sizes)
    [ "arithmetic", arithmetic; "application", application; "nested-let", nested_lets ]
;;

let now () = Unix.gettimeofday ()

let run_iterations iterations parse source =
  let started = now () in
  for _ = 1 to iterations do
    ignore (Sys.opaque_identity (parse source))
  done;
  now () -. started
;;

let choose_iterations parse source =
  let target_seconds = 0.1 in
  let maximum_iterations = 1_000_000 in
  let rec loop iterations =
    let elapsed = run_iterations iterations parse source in
    if elapsed >= target_seconds || iterations >= maximum_iterations
    then iterations
    else loop (min maximum_iterations (iterations * 2))
  in
  loop 1
;;

let median values =
  let sorted = Array.copy values in
  Array.sort Float.compare sorted;
  sorted.(Array.length sorted / 2)
;;

let minimum values = Array.fold_left Float.min Float.infinity values

let measure parser source =
  let warmup_runs = 5 in
  let sample_count = 15 in
  for _ = 1 to warmup_runs do
    ignore (Sys.opaque_identity (parser.parse source))
  done;
  let iterations = choose_iterations parser.parse source in
  let samples =
    Array.init sample_count (fun _ ->
      Gc.full_major ();
      run_iterations iterations parser.parse source /. float_of_int iterations)
  in
  { iterations; median_seconds = median samples; minimum_seconds = minimum samples }
;;

let verify case =
  let menhir_ast = menhir.parse case.source in
  let combinator_ast = combinator.parse case.source in
  if menhir_ast <> combinator_ast
  then failwith "the parsers produced different ASTs"
;;

let milliseconds seconds = seconds *. 1_000.

let megabytes_per_second ~bytes ~seconds =
  float_of_int bytes /. seconds /. 1_000_000.
;;

let print_header () =
  Printf.printf "OCaml: %s\n" Sys.ocaml_version;
  Printf.printf "samples: 15, warmup runs: 5, target sample time: 0.1 s\n\n";
  Printf.printf
    "%-12s %8s %9s %-11s %10s %11s %10s %10s\n"
    "case"
    "size"
    "bytes"
    "parser"
    "iterations"
    "median_ms"
    "min_ms"
    "MB/s";
  Printf.printf "%s\n" (String.make 92 '-')
;;

let print_measurement case parser measurement =
  let bytes = String.length case.source in
  Printf.printf
    "%-12s %8d %9d %-11s %10d %11.3f %10.3f %10.2f\n%!"
    case.family
    case.size
    bytes
    parser.name
    measurement.iterations
    (milliseconds measurement.median_seconds)
    (milliseconds measurement.minimum_seconds)
    (megabytes_per_second ~bytes ~seconds:measurement.median_seconds)
;;

let run_case index case =
  try
    verify case;
    let parsers = if index mod 2 = 0 then [ menhir; combinator ] else [ combinator; menhir ] in
    List.iter
      (fun parser ->
        try print_measurement case parser (measure parser case.source) with
        | exn ->
          Printf.printf
            "%-12s %8d %9d %-11s ERROR: %s\n%!"
            case.family
            case.size
            (String.length case.source)
            parser.name
            (Printexc.to_string exn))
      parsers
  with
  | exn ->
    Printf.printf
      "%-12s %8d %9d verification ERROR: %s\n%!"
      case.family
      case.size
      (String.length case.source)
      (Printexc.to_string exn)
;;

let () =
  print_header ();
  List.iteri run_case cases
