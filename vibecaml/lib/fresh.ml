[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let counter = ref 0

let name base =
  incr counter;
  Printf.sprintf "%s__%d" base !counter
;;

let reset () = counter := 0
