[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Benchmark
open Top_down
open Bottom_up

let top_down_parser = Top_down.Parser.parse_expr
let bottom_up_parser = Bottom_up.Parser.parse_expr

let read_file p =
  let ic = open_in p in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s
;;

let files = (fun i -> Printf.sprintf "examples/ast_expression_%d.txt" i) |> List.init 100

let () =
  let t = 1 in
  let repeat = 2 in
  let samples =
    List.concat_map
      (fun path ->
         let content = read_file path in
         throughputN
           ~repeat
           t
           [ ( "top_down_parser"
             , (fun s ->
                 let _ = top_down_parser s in
                 ())
             , content )
           ; ( "bottom_up_parser"
             , (fun s ->
                 let _ = bottom_up_parser s in
                 ())
             , content )
           ])
      files
  in
  print_gc samples
;;
