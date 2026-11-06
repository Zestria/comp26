[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Parser performance benchmark for Task 1 (front-end).

    Generates three families of inputs at runtime:
    - (a) a deep chain of nested [let] bindings;
    - (b) a flat left-associated sum;
    - (c) many top-level declarations.
    Each input is parsed [iterations] times with both parsers, measuring
    [Sys.time] (CPU time), and a table with input size, byte count, times
    and the topdown/bottomup ratio is printed.

    This is a rough experiment, not a micro-benchmark: [Sys.time] has
    ~ms resolution, so the sizes are chosen to keep each run measurable
    while keeping the total wall-clock time in the low seconds. *)

module Parse = Mylittleml.Frontend.Parse

let default_iterations = 10

(* Input generators ------------------------------------------------------ *)

(** (a) Deep chain: [let x0 = 1 in let x1 = x0 + x0 in ... in xN]. *)
let deep_chain n =
  "let x0 = 1 in "
  ^ String.concat
      ""
      (List.init
         (max 0 (n - 1))
         (fun i -> Printf.sprintf "let x%d = x%d + x%d in " (i + 1) i i))
  ^ Printf.sprintf "x%d" (n - 1)
;;

let flat_sum n = String.concat "+" (List.init n (fun _ -> "1"))

let many_decls n =
  String.concat "" (List.init n (fun i -> Printf.sprintf "let d%d = %d;;\n" i i))
;;

let rows =
  [ "deep-let", deep_chain, [ 100; 400; 900 ]
  ; "flat-sum", flat_sum, [ 1_000; 10_000; 30_000 ]
  ; "many-decls", many_decls, [ 500; 2_000; 4_000 ]
  ]
;;

(* Measurement ----------------------------------------------------------- *)

(** CPU time of repeated parses, or an explicit parser diagnostic. *)
let time_parse ~iterations kind text =
  let opts = { Parse.sticky = false } in
  let start = Sys.time () in
  let rec loop remaining =
    if remaining = 0
    then Ok (Sys.time () -. start)
    else Result.bind (Parse.parse kind opts text) (fun _ -> loop (remaining - 1))
  in
  loop iterations
;;

let measure kind text =
  match time_parse ~iterations:default_iterations kind text with
  | Ok time -> time
  | Error message ->
    prerr_endline ("benchmark: " ^ message);
    exit 1
;;

(* Table ----------------------------------------------------------------- *)

let () =
  Printf.printf "input          size    bytes   topdown(s)  bottomup(s)  ratio(td/bu)\n";
  List.iter
    (fun (label, gen, sizes) ->
       List.iter
         (fun size ->
            let text = gen size in
            let td = measure Parse.Topdown text in
            let bu = measure Parse.Bottomup text in
            Printf.printf
              "%-12s %8d %8d %12.6f %12.6f %9.2f\n"
              label
              size
              (String.length text)
              td
              bu
              (td /. bu))
         sizes)
    rows
;;
