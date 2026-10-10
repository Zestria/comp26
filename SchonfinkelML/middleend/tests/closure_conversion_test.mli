[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Tests for closure conversion: the expect-tests pin down the converted form
    of a source, and the structural helpers check the invariant that every
    abstraction of the result is closed. *)

open SchonfinkelML_frontend.Common

(** [parse s] is the expression [s] parses to; it raises [Failure] on a parse
    error, since the tests only use sources that parse. *)
val parse : string -> expr

(** [converted_of_string s] is the result of closure-converting [parse s]. *)
val converted_of_string : string -> expr

(** [print_converted s] prints the conversion of [s]. *)
val print_converted : string -> unit

(** [closed_expr env e] is [true] when every abstraction in [e] only refers to
    its own parameter and to the names in [env]. *)
val closed_expr : string list -> expr -> bool

(** [check_invariant source] converts [source] and reports whether every
    abstraction of the result is closed, printing the offending source and
    result when it is not. *)
val check_invariant : string -> bool

(** [string_contains haystack needle] is [true] when [needle] occurs in
    [haystack] as a substring. *)
val string_contains : string -> string -> bool

(** [exists_name_substr substr e] is [true] when some name bound or referenced
    in [e] contains [substr]. *)
val exists_name_substr : string -> expr -> bool

(** A CPS factorial in surface syntax, converted by the tests. *)
val cps_fact : string

(** A CPS fibonacci in surface syntax, converted by the tests. *)
val cps_fib : string

(** The sources the determinism and invariant tests run over. *)
val cases : string list
