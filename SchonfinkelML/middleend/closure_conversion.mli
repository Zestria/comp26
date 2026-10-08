[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common

val closure_convert : ?globals:string list -> expr -> expr
val closure_convert_program : ?globals:string list -> program -> program
val free_vars : ?globals:string list -> expr -> string list
