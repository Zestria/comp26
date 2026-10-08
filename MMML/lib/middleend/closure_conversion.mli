(** Closure conversion directly from parser AST. Lexical shadowing is resolved
    by scoped environments; no alpha-renaming or ANF is required. *)

type problem =
  | Unbound_identifier of string
  | Recursive_wildcard
  | Recursive_rhs_not_lambda of string

(** Structural context, from the top-level definition to the failing node.
    The parser AST has no source locations, so diagnostics do not invent them. *)
type error =
  { path : string list
  ; problem : problem
  }

(** External names are explicitly supplied by the caller; duplicates and input
    order have no effect. Lexical bindings take precedence over externals.
    Their runtime semantics, including primitives, belong to the IR consumer.

    Recursive definitions must bind a name to a lambda. This includes curried
    functions and recursion capturing outer values. Recursive wildcard and
    other recursive RHS forms return [Error]. The input AST is not mutated.

    The result retains nested code and unary calls. Captures are ordered by
    [String.compare]; creation reads existing bindings exactly once. No
    evaluation, hoisting, type inference or primitive lowering is performed. *)
val convert : externals:string list -> Ast.program -> (Closure_ir.program, error) result

val pp_error : Format.formatter -> error -> unit
