[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend

type dataset =
  { name : string
  ; size : int
  ; source : string
  }

let parse_ll source =
  match LL_parser.Parser.parse_string_LL source with
  | Ok ast ->
    let (_ : Ast.program) = Sys.opaque_identity ast in
    ()
  | Error _ -> failwith "the recursive-descent parser rejected a benchmark input"
;;

let parse_lr source =
  match Parse.parse source with
  | Ok ast ->
    let (_ : Ast.program) = Sys.opaque_identity ast in
    ()
  | Error message -> failwith ("the Menhir parser rejected a benchmark input: " ^ message)
;;

let check_dataset dataset =
  match LL_parser.Parser.parse_string_LL dataset.source, Parse.parse dataset.source with
  | Ok ll_ast, Ok lr_ast when ll_ast = lr_ast -> ()
  | Ok _, Ok _ ->
    failwith
      (Printf.sprintf "parsers produced different ASTs for dataset %S" dataset.name)
  | Error _, _ ->
    failwith
      (Printf.sprintf "the recursive-descent parser rejected dataset %S" dataset.name)
  | _, Error message ->
    failwith
      (Printf.sprintf "the Menhir parser rejected dataset %S: %s" dataset.name message)
;;

let make_arithmetic size =
  List.init size (fun i -> string_of_int (i mod 100)) |> String.concat " + "
;;

let make_application size =
  let arguments = List.init size (fun i -> "x" ^ string_of_int i) in
  String.concat " " ("f" :: arguments)
;;

let make_definitions size =
  List.init size (fun i -> Printf.sprintf "let x%d = %d" i (i mod 100))
  |> String.concat ";;"
;;

let datasets =
  [ 10; 100; 1_000; 5_000 ]
  |> List.concat_map (fun size ->
    [ { name = "arithmetic"; size; source = make_arithmetic size }
    ; { name = "application"; size; source = make_application size }
    ; { name = "definitions"; size; source = make_definitions size }
    ])
;;

let write_samples output dataset =
  check_dataset dataset;
  let samples =
    Benchmark.throughputN
      ~style:Benchmark.Nil
      ~repeat:5
      1
      [ "LL", parse_ll, dataset.source; "LR", parse_lr, dataset.source ]
  in
  List.iter
    (fun (parser, measurements) ->
       List.iteri
         (fun repetition sample ->
            let iterations = Int64.to_float sample.Benchmark.iters in
            let wall_seconds_per_parse = sample.wall /. iterations in
            Printf.fprintf
              output
              "%s,%d,%d,%s,%d,%Ld,%.12g,%.12g\n"
              dataset.name
              dataset.size
              (String.length dataset.source)
              parser
              repetition
              sample.iters
              sample.wall
              wall_seconds_per_parse)
         measurements)
    samples;
  flush output
;;

let () =
  let output_path =
    if Array.length Sys.argv > 1 then Sys.argv.(1) else "benchmark/benchmark_results.csv"
  in
  let output = open_out output_path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr output)
    (fun () ->
       output_string
         output
         "dataset,size,source_bytes,parser,repetition,iterations,total_wall_seconds,wall_seconds_per_parse\n";
       List.iter (write_samples output) datasets);
  Printf.printf "Benchmark results written to %s\n%!" output_path
;;
