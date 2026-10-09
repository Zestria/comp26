[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type atom =
  | Constant of Frontend.Constant.t
  | Var of Id.t
  | Primitive of Primitive.t

(** Common normal-form syntax; stages supply their own function payload. *)
type 'function_ computation =
  | Atom of atom
  | Tuple of atom * atom * atom list
  | Field of atom * int
  | Function of 'function_
  | Apply of atom * atom * atom list
  | If of atom * 'function_ term * 'function_ term

and 'function_ term =
  | Return of 'function_ computation
  | Let of Id.t * 'function_ computation * 'function_ term
  | Let_rec of Id.t * 'function_ term * 'function_ term
  (** The whole initializer remains within the recursive binding's scope. *)
  | Sequence of 'function_ computation * 'function_ term

type function_definition =
  { parameter : Id.t
  ; parameters : Id.t list
  ; body : function_definition term
  }

type expression = function_definition term
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

  (** [error] embeds common binding errors in the stage's diagnostic type;
      the callback checks function payloads and their stage-specific scopes. *)
  val term
    :  error:(Lambda.invariant_error -> 'error)
    -> (state -> Id.Set.t -> 'function_ -> (state, 'error) result)
    -> state
    -> Id.Set.t
    -> 'function_ term
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
  -> function_:('source -> 'target)
  -> 'source term
  -> 'target term

val map_accum
  :  atom:(atom -> atom)
  -> function_:('state -> 'source -> 'state * 'target)
  -> 'state
  -> 'source term
  -> 'state * 'target term

(** Traverse in the same order, stopping at the first callback error. *)
val map_accum_result
  :  atom:(atom -> atom)
  -> function_:('state -> 'source -> ('state * 'target, 'error) result)
  -> 'state
  -> 'source term
  -> ('state * 'target term, 'error) result

(** Collect information without rebuilding the term. Binders precede their
    initializers; operands and branches are visited from left to right.
    The function callback handles traversal inside its payload. *)
val fold
  :  atom:('state -> atom -> 'state)
  -> binder:('state -> Id.t -> 'state)
  -> function_:('state -> 'function_ -> 'state)
  -> 'state
  -> 'function_ term
  -> 'state

(** Normalize a term satisfying [Lambda.validate], propagating atomic aliases
    and avoiding unnecessary bindings for tail results.
    Continue the supply used to build the input; return the supply for later passes. *)
val of_lambda
  :  Id.supply
  -> Lambda.expression
  -> (expression * Id.supply, Id.error) result

(** Embed normalized terms in Lambda for independent execution in tests. *)
val to_lambda : expression -> Lambda.expression
