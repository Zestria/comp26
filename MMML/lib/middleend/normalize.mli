(** Deterministic, capture-free normalization with fresh state per call.
    Application evaluates the function, argument, then the unary call.
    Function bodies and conditional branches retain their evaluation boundary.
    Atomic local/constant/builtin aliases disappear; external lookups and
    wildcard computations are retained so lookup errors/effects keep their order.

    [bindings] replaces the initial environment (default: standard operators).
    A caller may map source names to external references to override builtins.
    Unlisted free names are preserved as external references. Local IDs in
    caller bindings require an enclosing scope and should not be used here.

    Saturated known builtin AND/OR calls short-circuit, including simple aliases.
    Unknown higher-order and previously partial applications remain strict.
    Recursion supports only variable-bound syntactic lambda RHSs. *)
type error =
  | Recursive_wildcard
  | Recursive_non_function of string

val expression : ?bindings:(string * Anf.imm) list -> Ast.expr -> (Anf.term, error) result

(** Program definitions initialize in source order with one fresh-ID supply.
    Nonrecursive RHSs see earlier bindings; recursive bodies see themselves. *)
val program
  :  ?bindings:(string * Anf.imm) list
  -> Ast.program
  -> (Anf.program, error) result

val pp_error : Format.formatter -> error -> unit
