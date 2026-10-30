[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Shmel_backend

let expect_error = function
  | Error _ -> ()
  | Ok _ -> failwith "expected an invalid closure layout to be rejected"
;;

let () =
  let f = Result.get_ok (Closure.create ~code:"lifted_fun0" ~arity:3) in
  assert (String.equal (Closure.code f) "lifted_fun0");
  assert (Closure.arity f = 3);
  assert (Closure.word_bytes = 8);
  assert (Closure.code_offset = 0);
  assert (Closure.arity_offset = 8);
  assert (Closure.applied_offset = 16);
  assert (Closure.arguments_offset = 24);
  assert (Closure.size_bytes ~applied:0 = Ok 24);
  assert (Closure.size_bytes ~applied:2 = Ok 40);
  assert (Closure.argument_offset 0 = Ok 24);
  assert (Closure.argument_offset 1 = Ok 32);
  assert (Closure.remaining_arity f ~applied:0 = Ok 3);
  assert (Closure.remaining_arity f ~applied:2 = Ok 1);
  let wide = Result.get_ok (Closure.create ~code:"eight_args" ~arity:8) in
  assert (Closure.arity wide = 8);
  assert (Closure.remaining_arity wide ~applied:7 = Ok 1);
  assert (Closure.size_bytes ~applied:7 = Ok 80);
  assert (Closure.argument_offset 7 = Ok 80);
  let largest_arity =
    Result.get_ok (Closure.create ~code:"largest_arity" ~arity:Closure.max_arity)
  in
  assert (Closure.max_arity = 8);
  assert (Closure.arity largest_arity = Closure.max_arity);
  expect_error (Closure.create ~code:"too_wide" ~arity:(Closure.max_arity + 1));
  expect_error (Closure.create ~code:"" ~arity:1);
  expect_error (Closure.create ~code:"f" ~arity:0);
  expect_error (Closure.create ~code:"f" ~arity:(-1));
  expect_error (Closure.remaining_arity f ~applied:(-1));
  expect_error (Closure.remaining_arity f ~applied:3);
  expect_error (Closure.remaining_arity f ~applied:4);
  expect_error (Closure.argument_offset (-1));
  expect_error (Closure.size_bytes ~applied:(-1));
  expect_error (Closure.size_bytes ~applied:max_int);
  let largest = (max_int - Closure.arguments_offset) / Closure.word_bytes in
  assert (Result.is_ok (Closure.size_bytes ~applied:largest));
  expect_error (Closure.size_bytes ~applied:(largest + 1))
;;
