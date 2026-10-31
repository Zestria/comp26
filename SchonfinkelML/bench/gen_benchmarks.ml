[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Gen = QCheck.Gen
open Ast_gen
open Arg
open SchonfinkelML_frontend.Common

[@@@zanuda "-mutability_check"] (* For Config lib *)

type config =
  { mutable height : int
  ; mutable seed : int
  ; mutable filecount : int
  }

let write_generated_file ~n ~st ~dir ~gen =
  if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;
  let expr = Gen.generate1 ~rand:st gen in
  let code = Format.asprintf "%a" pp_expr expr in
  let filename = Printf.sprintf "ast_expression_%d.txt" n in
  let path = Filename.concat dir filename in
  let oc = open_out path in
  output_string oc code;
  close_out oc
;;

let usage_msg = "usage: gen_benchmarks [OPTIONS]"

let flag_specs cfg =
  [ ( "-n"
    , Arg.Int (fun n -> cfg.filecount <- n)
    , "<num>  number of file to generate(default: 100)" )
  ; "-s", Arg.Int (fun n -> cfg.seed <- n), "<num>  set seed (default: random)"
  ; ( "--height"
    , Arg.Int (fun n -> cfg.filecount <- n)
    , "<num> height of generated expression (default: random)" )
  ]
;;

let () =
  let cfg = { height = 100; seed = 42; filecount = 100 } in
  let gen = Ast_gen.gen_expr cfg.height in
  let st = Random.State.make [| cfg.seed |] in
  let specs = flag_specs cfg in
  let anon = fun s -> () in
  Arg.parse specs anon usage_msg;
  Format.printf "Begin\n";
  let rec helper n cfg =
    if n >= cfg.filecount
    then ()
    else (
      Format.printf "Generating file number %d...\n" n;
      write_generated_file ~n ~st ~dir:"./examples" ~gen;
      helper (n + 1) cfg)
  in
  let res = helper 0 cfg in
  Format.printf "Generation complete!\n";
  Format.printf "Seed: %d\n" cfg.seed;
  res
;;
