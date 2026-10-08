(** Logical closures before hoisting and ANF. Source names need not be unique.
    [Self] and [Env_slot] address the currently invoked closure, not a source
    binding. A call passes that closure and one argument to its code. *)

type reference =
  | Local of string
  | External of string
  | Env_slot of int
  | Self

type expr =
  | Constant of Ast.constant
  | Reference of reference
  | Make_closure of code * reference list
  | Call of expr * expr
  | Let of Ast.pattern * expr * expr
  | If of expr * expr * expr

(** Capture names are diagnostic labels; list order defines zero-based slots.
    Capture references are read in the creation scope. The code body starts
    a fresh local scope with only its parameter. Nested code has its own slots.
    Restricting captures to references prevents evaluating an arbitrary RHS
    again when creating a closure. *)
and code =
  { parameter : Ast.pattern
  ; capture_names : string list
  ; body : expr
  }

type definition = Definition of Ast.pattern * expr

(** Definitions bind sequentially. External identities are independent of
    later local shadowing; their runtime semantics belong to the consumer. *)
type program =
  { externals : string list
  ; definitions : definition list
  }

type validation_problem =
  | Unbound_local of string
  | Undeclared_external of string
  | Environment_outside_code
  | Self_outside_code
  | Invalid_slot of int * int
  | Capture_count_mismatch of int * int
  | Duplicate_capture_name of string

type validation_error =
  { path : string list
  ; problem : validation_problem
  }

(** Check lexical closure, capture layouts and external references, including
    every nested code body. This is not type inference or an arity checker. *)
val validate : program -> (unit, validation_error list) result

val pp_validation_error : Format.formatter -> validation_error -> unit

(** Deterministic diagnostic rendering, not reparsable MiniML source. *)
val pp_program : Format.formatter -> program -> unit
