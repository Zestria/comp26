[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Specialize calls with a known code label, exact declared arity and no
    required environment. For example, [let inc x = x + 1 in inc 41] needs
    no function-value allocation; a partial application or a returned [inc]
    still needs its closure. A recursively called [f] may become closed, but
    returning or partially applying [f] keeps its value available.

    Known identities propagate through every construction's capture-to-slot
    correspondence. Conflicting or dynamic incoming values remain unknown.
    Facts propagate along capture dependencies; an identity changes at most
    from unseen to known to unknown. A separate demand graph links slots to
    their owning environment, and a callee's environment to the slot supplying
    that callee. Reachability from ordinary value uses identifies required
    slots, including through CPS continuations. A callee-only recursive cycle
    has no such root and can be closed. Both worklists read unchanged input;
    neither rescans whole function bodies to compute its fixed point.

    Partial applications and overapplications retain indirect calls.

    Input must satisfy [Lifted.validate] and come from recursively safe source
    definitions, checked by [Recursive_check] before lowering erases patterns
    (as in [Pipeline]). This does not repair arbitrary scope-valid IR that
    calls an uninitialized recursive value. Environment slots and corresponding
    captures are removed together. Effectful initializers are preserved. *)
val optimize : Lifted.program -> Lifted.program
