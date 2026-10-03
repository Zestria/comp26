[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let counter = ref 0

let fresh_ident base =
  incr counter;
  Printf.sprintf "%s_%d" base !counter
;;

let reset () = counter := 0
