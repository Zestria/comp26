[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Expect tests for the conversion to A-normal form: [print_anf] converts and
    prints one expression, and the [let%expect_test]s of the module pin down the
    expected form. *)

open SchonfinkelML_frontend.Common

(** [print_anf e] prints [Anf.string_of_aexpr (Anf.create_anf e)]. *)
val print_anf : expr -> unit
