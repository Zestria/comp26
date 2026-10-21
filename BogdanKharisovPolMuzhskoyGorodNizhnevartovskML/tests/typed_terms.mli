[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Independent, monomorphic, simply typed test terms. The GADT keeps expression
    result types; generation and substitution preserve lexical binding identity.
    There is no recursion. Integer results are restricted to [-1_000_000, 1_000_000]
    and the reference evaluator has a 20_000-step operational budget. Generated
    types have depth at most four; the shrinking measure is at most 600. *)
type t

val arbitrary : t QCheck.arbitrary
val source : t -> string
val expected : t -> string
val size : t -> int
val same_type : t -> t -> bool
val validate : t -> (unit, string) result
val shrink : t QCheck.Shrink.t

(** Features encountered along executed paths, including invoked closures,
    partial applications of multi-parameter functions, and reads of a binding
    that shadows an older name. Saturation and overapplication are distinct. *)
val execution_features : t -> string list

(** A closed term with nested scopes and removable structure, used to exercise
    QCheck's complete shrinking loop against an arithmetic mutation. *)
val shrink_witness : unit -> t
