[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_middleend
open SchonfinkelML_backend

let lower src =
  match Top_down.Parser.parse_expr src with
  | Error e -> failwith (Top_down.Parser.string_of_error e)
  | Ok e ->
    e
    |> Closure_conversion.closure_convert
    |> Lambda_lifting.lift
    |> Anf.create_anf
    |> Closures.lower
    |> Anf.string_of_aexpr

let contains needle haystack =
  let nl = String.length needle and hl = String.length haystack in
  let rec go i = i + nl <= hl && (String.sub haystack i nl = needle || go (i + 1)) in
  go 0

let check name src needle should_contain =
  let s = lower src in
  let has = contains needle s in
  if has <> should_contain then (
    Printf.printf "FAIL %s\n  input: %s\n  expected %s %S\n  got: %s\n"
      name src (if should_contain then "to contain" else "NOT to contain") needle s;
    exit 1)

let () =
  check "id_full"         "let f = fun x -> x in f 5"                    "mk_closure" false;
  check "capture"         "let y = 1 in let f = fun x -> x + y in f 5"   "mk_closure" true;
  check "curried_full"    "let add = fun x -> fun y -> x + y in add 5 3" "mk_closure" false;
  check "curried_partial" "let add = fun x -> fun y -> x + y in add 5"   "mk_closure" true;
  check "call_var"        "let g = fun f -> f 5 in g (fun x -> x)"       "apply"      true;
  check "cps_capture"     "let k = fun x -> x in let f = fun n -> fun r -> k (n * r) in f 1"
                                                                          "mk_closure" true;
  print_endline "test_closures: OK"