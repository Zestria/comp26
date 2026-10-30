[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Layout of immutable heap closures for RV64. A function value is a pointer
    to this object, not directly to its code. See [closure_runtime.h].

    Each field occupies eight bytes: code address, total arity, number of
    applied arguments, followed by the applied arguments in source order.
    A valid function closure has [0 <= applied < arity]. Saturated applications
    call the code rather than construct a saturated closure.

    The code entry follows the standard RV64 integer psABI: up to eight
    word arguments are passed in [a0] .. [a7], and the word result in [a0].
    No implicit environment parameter is added;
    saved arguments form the prefix of the ordinary parameter list. *)

(** Compile-time description of the original function; not a runtime object. *)
type t

(** Maximum code arity supported by the C runtime, including captures. *)
val max_arity : int

(** Rejects an empty code label and arities outside [1 .. max_arity].
    Arity counts all parameters, including explicit captures. *)
val create : code:Riscv.label -> arity:int -> (t, string) result

val code : t -> Riscv.label
val arity : t -> int
val word_bytes : int
val code_offset : int
val arity_offset : int
val applied_offset : int
val arguments_offset : int

(** Byte offset of a saved argument, starting at index zero. Rejects negative
    indices and offsets that cannot be represented as an OCaml [int]. *)
val argument_offset : int -> (int, string) result

(** Allocation size for a closure containing [applied] saved arguments.
    Rejects negative counts and sizes overflowing an OCaml [int]. This only
    computes layout size; use [remaining_arity] to validate against a function. *)
val size_bytes : applied:int -> (int, string) result

(** Number of arguments still needed. Rejects invalid or saturated closures. *)
val remaining_arity : t -> applied:int -> (int, string) result
