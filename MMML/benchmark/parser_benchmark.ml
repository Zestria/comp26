[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Comparative benchmark for the recursive-descent and Menhir parsers. *)

type parser =
  { name : string
  ; parse : string -> Ast.program Error_monad.t
  }

type measurement =
  { repetitions : int
  ; seconds : float
  }

exception Parse_error of string * Error_monad.error

let top_down = { name = "top-down"; parse = Topdown_parser.parse_program }
let bottom_up = { name = "bottom-up (Menhir)"; parse = Parser_menhir.parse_program }

let parse parser source =
  match parser.parse source with
  | Ok program -> program
  | Error error -> raise (Parse_error (parser.name, error))
;;

let run_repeatedly repetitions operation =
  let rec loop remaining =
    if remaining > 0
    then (
      let _result = Sys.opaque_identity (operation ()) in
      loop (remaining - 1))
  in
  loop repetitions
;;

let elapsed repetitions operation =
  let started_at = Unix.gettimeofday () in
  run_repeatedly repetitions operation;
  Unix.gettimeofday () -. started_at
;;

let measure ~minimum_seconds operation =
  Gc.compact ();
  run_repeatedly 3 operation;
  let rec calibrate repetitions =
    let seconds = elapsed repetitions operation in
    if seconds >= minimum_seconds || repetitions >= 1_024
    then repetitions
    else calibrate (repetitions * 2)
  in
  let repetitions = calibrate 1 in
  (* Calibration allocates parser results. Compact once more outside the timed
     interval so that both parsers begin their measured run in the same state. *)
  Gc.compact ();
  let seconds = elapsed repetitions operation in
  { repetitions; seconds }
;;

let milliseconds_per_run measurement =
  measurement.seconds *. 1_000. /. Float.of_int measurement.repetitions
;;

let validate_parser workload parser =
  let actual = parse parser workload.Workload.source in
  let expected = workload.Workload.program in
  if not (String.equal (Ast.show_program actual) (Ast.show_program expected))
  then (
    Format.eprintf "%s parser produced an unexpected AST.\n%!" parser.name;
    exit 2)
;;

let benchmark workload =
  let bytes = String.length workload.Workload.source in
  validate_parser workload top_down;
  validate_parser workload bottom_up;
  let top_down_result =
    measure ~minimum_seconds:0.5 (fun () -> parse top_down workload.source)
  in
  let bottom_up_result =
    measure ~minimum_seconds:0.5 (fun () -> parse bottom_up workload.source)
  in
  let top_down_ms = milliseconds_per_run top_down_result in
  let bottom_up_ms = milliseconds_per_run bottom_up_result in
  let difference_percent = ((bottom_up_ms /. top_down_ms) -. 1.) *. 100. in
  Format.printf
    "%11d  %10d  %11.3f  %12.3f  %+12.2f%%\n%!"
    workload.definitions
    bytes
    top_down_ms
    bottom_up_ms
    difference_percent
;;

let main () =
  Format.printf "   definitions|   bytes |  top-down ms |  bottom-up ms |   difference \n";
  List.iter (fun size -> size |> Workload.create |> benchmark) Workload.default_sizes
;;

let () =
  try main () with
  | Parse_error (parser, error) ->
    Format.eprintf "%s parser failed: %a\n%!" parser Error_monad.pp_error error;
    exit 2
;;
