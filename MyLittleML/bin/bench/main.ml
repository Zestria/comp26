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

module Parse = Parsing.Parse

let default_iterations = 10

(* Input generators ------------------------------------------------------ *)

(** (a) Deep chain: [let x0 = 1 in let x1 = x0 + x0 in ... in xN]. *)
let deep_chain n =
  let b = Buffer.create (n * 32) in
  Buffer.add_string b "let x0 = 1 in ";
  for i = 1 to n - 1 do
    Printf.bprintf b "let x%d = x%d + x%d in " i (i - 1) (i - 1)
  done;
  Printf.bprintf b "x%d" (n - 1);
  Buffer.contents b
;;

(** (b) Flat sum: [1+1+...+1]. *)
let flat_sum n =
  let b = Buffer.create (n * 2) in
  for i = 0 to n - 1 do
    if i > 0 then Buffer.add_char b '+';
    Buffer.add_string b "1"
  done;
  Buffer.contents b
;;

(** (c) Many top-level declarations. *)
let many_decls n =
  let b = Buffer.create (n * 16) in
  for i = 0 to n - 1 do
    Printf.bprintf b "let d%d = %d;;\n" i i
  done;
  Buffer.contents b
;;

let rows =
  [ ("deep-let", deep_chain, [ 100; 400; 900 ])
  ; ("flat-sum", flat_sum, [ 1_000; 10_000; 30_000 ])
  ; ("many-decls", many_decls, [ 500; 2_000; 4_000 ])
  ]
;;

(* Measurement ----------------------------------------------------------- *)

(** CPU time of [iterations] parses of [text] with [kind].  Raises if the
    generated input is rejected, so a wrong generator fails loudly. *)
let time_parse ~iterations kind text =
  let opts = { Parse.sticky = false } in
  let t0 = Sys.time () in
  for _ = 1 to iterations do
    match Parse.parse kind opts text with
    | Ok _ -> ()
    | Error msg ->
      failwith (Format.sprintf "%s parser rejected benchmark input: %s"
                  (match kind with
                   | Parse.Topdown -> "topdown"
                   | Parse.Bottomup -> "bottomup")
                  msg)
  done;
  Sys.time () -. t0
;;

(* Table ----------------------------------------------------------------- *)

let () =
  Printf.printf "input          size    bytes   topdown(s)  bottomup(s)  ratio(td/bu)\n";
  List.iter
    (fun (label, gen, sizes) ->
      List.iter
        (fun size ->
          let text = gen size in
          let td = time_parse ~iterations:default_iterations Parse.Topdown text in
          let bu = time_parse ~iterations:default_iterations Parse.Bottomup text in
          Printf.printf "%-12s %8d %8d %12.6f %12.6f %9.2f\n"
            label size (String.length text) td bu (td /. bu))
        sizes)
    rows
;;