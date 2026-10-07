(** Structural scope validation, not type inference. Stamps must be globally
    unique, even across disjoint branches/function bodies. External references
    must be explicitly permitted. Builtins need no external declarations. *)
type error =
  | Duplicate_binder of Anf.id
  | Unbound_local of Anf.id
  | Unbound_external of string

val term : externals:string list -> Anf.term -> (unit, error) result
val program : externals:string list -> Anf.program -> (unit, error) result
val pp_error : Format.formatter -> error -> unit
