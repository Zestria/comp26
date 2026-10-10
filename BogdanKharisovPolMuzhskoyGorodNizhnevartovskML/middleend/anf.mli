[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type atom =
  | Constant of Frontend.Constant.t
  | Var of Id.t
  | Primitive of Primitive.t

(** Common normal-form syntax; stages supply their own function payload and callee type. *)
type ('function_, 'callee) computation =
  | Atom of atom
  | Tuple of atom * atom * atom list
  | Field of atom * int
  | Function of 'function_
  | Apply of 'callee * atom * atom list
  | If of atom * ('function_, 'callee) term * ('function_, 'callee) term

and ('function_, 'callee) term =
  | Return of ('function_, 'callee) computation
  | Let of Id.t * ('function_, 'callee) computation * ('function_, 'callee) term
  | Let_rec of Id.t * ('function_, 'callee) term * ('function_, 'callee) term
  (** The whole initializer remains within the recursive binding's scope. *)
  | Sequence of ('function_, 'callee) computation * ('function_, 'callee) term

type function_definition =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : (function_definition, atom) term
  }

type expression = (function_definition, atom) term
type program = expression

(** Shared binding checks for ANF and the stages using its term syntax. *)
module Scope : sig
  (** Globally encountered binders, distinct from the lexical scope passed
      separately to checks. Successful checks return the accumulated state. *)
  type state

  val bind_all
    :  state
    -> Id.Set.t
    -> Id.t list
    -> (state * Id.Set.t, Lambda.invariant_error) result

  val atoms : Id.Set.t -> atom list -> (unit, Lambda.invariant_error) result
  val callee : Id.Set.t -> atom -> arity:int -> (unit, Lambda.invariant_error) result

  (** [error] embeds common binding errors in the stage's diagnostic type.
      Callbacks check callees and function payloads with their stage-specific
      scopes; ordinary argument atoms are always checked by the shared code. *)
  val term
    :  error:(Lambda.invariant_error -> 'error)
    -> callee:(Id.Set.t -> 'callee -> arity:int -> (unit, 'error) result)
    -> (state -> Id.Set.t -> 'function_ -> (state, 'error) result)
    -> state
    -> Id.Set.t
    -> ('function_, 'callee) term
    -> (state, 'error) result

  val run : (state -> (state, 'error) result) -> (unit, 'error) result
end

(** Check lexical scope, global uniqueness of binding occurrences and field
    index nonnegativity. Types and recursive initialization are separate checks. *)
val validate : expression -> (unit, Lambda.invariant_error) result

(** Traverse ordinary terms from left to right. Function payloads and their
    bodies are handled by the supplied callback. *)
val map
  :  atom:(atom -> atom)
  -> callee:('source_callee -> arity:int -> 'target_callee)
  -> function_:('source -> 'target)
  -> ('source, 'source_callee) term
  -> ('target, 'target_callee) term

val map_accum
  :  atom:(atom -> atom)
  -> callee:('source_callee -> arity:int -> 'target_callee)
  -> function_:('state -> 'source -> 'state * 'target)
  -> 'state
  -> ('source, 'source_callee) term
  -> 'state * ('target, 'target_callee) term

(** Traverse in the same order, stopping at the first callback error. *)
val map_accum_result
  :  atom:(atom -> atom)
  -> callee:('source_callee -> arity:int -> 'target_callee)
  -> function_:('state -> 'source -> ('state * 'target, 'error) result)
  -> 'state
  -> ('source, 'source_callee) term
  -> ('state * ('target, 'target_callee) term, 'error) result

(** Collect information without rebuilding the term. Binders precede their
    initializers; operands and branches are visited from left to right.
    The function callback handles traversal inside its payload. *)
val fold
  :  atom:('state -> atom -> 'state)
  -> callee:('state -> 'callee -> arity:int -> 'state)
  -> binder:('state -> Id.t -> 'state)
  -> function_:('state -> 'function_ -> 'state)
  -> 'state
  -> ('function_, 'callee) term
  -> 'state

(** Remove unused pure allocations; keep calls and unknown field inspections.
    The function callback owns any stage-specific body or capture uses. Direct
    code labels are not value-variable uses. A recursive initializer is removed
    only when it consists solely of an unused function construction. *)
val remove_unused
  :  callee:(Id.Set.t -> 'callee -> arity:int -> Id.Set.t)
  -> function_:
       ((Id.Set.t -> ('function_, 'callee) term -> Id.Set.t * ('function_, 'callee) term)
        -> Id.Set.t
        -> 'function_
        -> Id.Set.t * 'function_)
  -> ('function_, 'callee) term
  -> ('function_, 'callee) term

(** Normalize a term satisfying [Lambda.validate]. Propagate atomic aliases and
    known immutable tuple fields, fold proven-total constant primitives through
    [Primitive_simplify], select constant boolean branches, avoid tail
    result temporaries and remove unused allocations, including syntactic recursive
    functions with no uses outside their initializer. Remaining calls and
    unknown projections remain eager even if their results are unused.
    Recursive initialization safety must be checked before this transformation;
    recursive initializers retain their scope and do not export value facts.
    Continue the supply used to build the input; return the supply for later passes. *)
val of_lambda
  :  Id.supply
  -> Lambda.expression
  -> (expression * Id.supply, Id.error) result

(** Embed normalized terms in Lambda for independent execution in tests. *)
val to_lambda : expression -> Lambda.expression
