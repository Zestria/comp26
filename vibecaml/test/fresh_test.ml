[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml

let () =
  Fresh.reset ();
  let first = Fresh.name "t" in
  let second = Fresh.name "t" in
  let other = Fresh.name "lifted" in
  assert (first = "t__1");
  assert (second = "t__2");
  (* Счётчик общий для всех основ: имена разных проходов не пересекаются. *)
  assert (other = "lifted__3");
  Fresh.reset ();
  assert (Fresh.name "t" = "t__1")
;;
