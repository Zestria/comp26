[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common

(* Stupidest register allocator ever award goes to... *)

module Allocator = struct
  let registers =
    [ T0; T1; T2; T3; T4; T5; T6; S1; S2; S3; S4; S5; S6; S7; S8; S9; S10; S11 ]
  ;;
end
