[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* End-to-end checks for the backend: a MiniML source is compiled through the
   frontend and middleend to asm text, which must contain the expected
   instruction shapes for the MMML runtime contract. *)

let externals =
  "print_int" :: List.map Middleend.Anf.builtin_name Middleend.Anf.builtins
;;

let compile source =
  let ( let* ) = Result.bind in
  let* program =
    Result.map_error Error_monad.show_error (Topdown_parser.parse_program source)
  in
  let* program =
    Result.map_error
      (fun error -> Format.asprintf "%a" Middleend.Closure_conversion.pp_error error)
      (Middleend.Closure_conversion.convert ~externals program)
  in
  let* program =
    Result.map_error
      (fun error -> Format.asprintf "%a" Middleend.Normalize.pp_error error)
      (Middleend.Normalize.program program)
  in
  Ok (Backend.Asm_conversion.compile program |> Backend.Codegen.string_of_program)
;;

let contains_substring haystack needle =
  let needle_length = String.length needle in
  let haystack_length = String.length haystack in
  let rec search index =
    if index + needle_length > haystack_length
    then false
    else if String.sub haystack index needle_length = needle
    then true
    else search (index + 1)
  in
  search 0
;;

let expect_contains source needles =
  match compile source with
  | Error message ->
    Format.eprintf "asm_ast test: compile of %S failed: %s@." source message;
    exit 1
  | Ok output ->
    List.iter
      (fun needle ->
         if not (contains_substring output needle)
         then (
           Format.eprintf
             "asm_ast test: expected %S in@.%s"
             needle
             output;
           exit 1))
      needles
;;

let () =
  expect_contains "let answer = 1 + 2" [ "mmml_program_main"; "call mmml_int" ];
  expect_contains
    "let f x = x + 1\nlet answer = f 41"
    [ ".globl mmml_program_main"; "call mmml_apply"; "call mmml_make_closure" ];
  expect_contains
    "let rec fac n k =\n\
    \  if n <= 1 then k 1\n\
    \  else fac (n - 1) (fun r -> k (n * r))\n\
    \ let answer = fac 5 (fun x -> print_int x)"
    [ "call mmml_print_int"; "call mmml_make_closure" ];
  print_string "MMML backend: asm tests passed\n";
  flush stdout
;;
