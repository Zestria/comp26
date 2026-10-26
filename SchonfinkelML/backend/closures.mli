[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Anf = SchonfinkelML_middleend.Anf

(** [lower a] rewrites every [CApp] in [a] into one of three forms:
    - [CApp] — full application of a top-level name (arity matches);
    - [CMkClosure] — partial application of a top-level name;
    - [CApplyClosure] — call through a variable or computed value. *)
val lower : Anf.aexpr -> Anf.aexpr
