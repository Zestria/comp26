[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Data layout and runtime interface shared by generated code and
    [runtime/runtime.c]. Must stay in sync with that file.

    Values are 64-bit words. An immediate has its low bit set: the integer [n]
    is [2n + 1] (63-bit, arithmetic wraps modulo 2^63), [false] and [()] are
    [1], [true] is [3]. A heap value is the 8-aligned address of its header
    word, so its low bit is clear. The low bit lets the untyped structural
    comparison distinguish immediates from blocks.

    Header word: [field_count lsl 1] for a tuple, [1] for a closure. Compare
    needs only the kind (to reject functions) and the tuple size.

    Tuple: [header; v_0; ...; v_{n-1}].
    Closure: [header = 1; code address; arity (raw); env_0; ...; env_{k-1}].
    A partial application is a closure whose code is [rt_pap_code], arity is
    the number of missing arguments, and environment is
    [f; m (raw); a_1; ...; a_m].

    Calling convention for code labels: [a0] = the callee closure (undefined
    for [Direct] calls, whose environment is empty); argument [i] (1-based)
    in [a{i}] for [i <= 7]; argument [i > 7] in the caller's outgoing area
    at [8 * (i - 8)(sp)] at the call; result in [a0]. [s0]-[s11], [sp] are
    preserved, [sp] is 16-byte aligned at every call; [ra], [t*], [a*] are
    clobbered. This is the RISC-V LP64 C convention for
    [value code(value closure, value a_1, ..., value a_n)], so the runtime
    can call and implement code labels as C functions. *)

val word : int

(** Byte offsets from a heap value. *)

val tuple_field : int -> int

val closure_code : int
val closure_arity : int
val closure_env : int -> int

(** Header words. *)

val tuple_header : int -> int64

val closure_header : int64

(** Tagged immediates. *)

val unit_value : int64

val bool_value : bool -> int64

(** Runtime symbols ([runtime/runtime.c]).
    - [rt_alloc(long words) -> value]: fresh uninitialised block of [words]
      words (header included), from [malloc]; out of memory prints
      ["Fatal error: out of memory"] and exits with code 2. No collector:
      blocks live until the process exits.
    - [rt_apply(value f, long n, value *argv) -> value]: generic application
      of [f] to [n >= 1] arguments: exact, partial (allocates a PAP) and
      over-application (applies the result to the rest).
    - [rt_compare(value a, value b) -> long]: structural order, raw [-1], [0]
      or [1] (not tagged). Fields are compared left to right and the first
      difference decides; reaching a closure on either side, even the same
      pointer, raises [Invalid_argument "compare: functional value"].
    - [rt_print_int(value n) -> value]: prints the integer in decimal without
      a newline to buffered stdout, returns [()].
    - [rt_div_by_zero()]: never returns; uncaught [Division_by_zero].
    Uncaught runtime errors flush stdout, print
    ["Fatal error: exception ..."] to stderr and exit with code 2.
    The C [main] calls [ml_main], flushes stdout and returns 0. *)

val alloc : string

val apply : string
val compare : string
val print_int : string
val div_by_zero : string

(** Exported by generated code: evaluates the program body. *)
val entry : string
