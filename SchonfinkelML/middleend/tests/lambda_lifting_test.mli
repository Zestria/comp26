[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Tests for lambda lifting: the pass runs after closure conversion, so the
    helpers convert first and then lift, and the shape check asserts that no
    abstraction is left outside the right-hand side of a binding. *)

open SchonfinkelML_frontend.Common

(** [lift_of_string s] parses [s], closure-converts it and lifts the result; it
    raises [Failure] on a parse error, since the tests only use sources that
    parse. *)
val lift_of_string : string -> expr

(** [print_lifted s] prints the lifting of [s]. *)
val print_lifted : string -> unit

(** [chain_ok e] is [true] when [e] is a (possibly curried) abstraction whose
    body is an acceptable lifted expression: an abstraction may only occur as the
    right-hand side of a binding or as the body of such a chain. *)
val chain_ok : expr -> bool

(** [expr_ok e] is [true] when [e] contains no abstraction outside the
    right-hand side of a binding. *)
val expr_ok : expr -> bool

(** [print_shape s] prints whether every lambda of the lifting of [s] is
    let-bound. *)
val print_shape : string -> unit

(** A CPS factorial in surface syntax, lifted by the tests. *)
val cps_fact : string

(** A CPS fibonacci in surface syntax, lifted by the tests. *)
val cps_fib : string
