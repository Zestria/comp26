[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Untyped ANF. Every operator/call operand and branch condition is immediate.
    Only function bodies and conditional branches contain nested terms. *)
type id =
  { stamp : int
  ; hint : string
  }
(** [stamp] is the identity; [hint] is only for diagnostics. Stamps are unique
    throughout a normalized expression/program, including different branches. *)

type unary =
  | Negate
  | Positive

type binary =
  | Add
  | Subtract
  | Multiply
  | Divide
  | Equal
  | Not_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal

type builtin =
  | Unary of unary
  | Binary of binary
  | And
  | Or

type imm =
  | Constant of Ast.constant
  | Local of id
  | External of string
  | Builtin of builtin
  (** External references occupy a separate namespace from local IDs.
    [Builtin] records a resolved standard binding, not its source spelling. *)

type compound =
  | Value of imm
  | Apply of imm * imm
  | Unary_primitive of unary * imm
  | Binary_primitive of binary * imm * imm
  | Lambda of id * term
  | If of imm * term * term

and term =
  | Result of compound
  | Let of id * compound * term
  | Let_rec of id * id * term * term
  (** [Result] is a tail expression, not necessarily an immediate.
    In [Let] the binder is in scope only in the continuation.
    [Let_rec (fn, parameter, body, continuation)] binds [fn] in both terms,
    and [parameter] only in [body]. Lambda/recursion remain unary. *)

type definition =
  | Definition of id * term
  | Recursive_definition of id * id * term

(** Definitions initialize in list order. A nonrecursive definition's binder
    is not available in its own RHS. Recursive definitions bind functions only. *)
type program = definition list

val builtin_name : builtin -> string
val builtins : builtin list
val default_bindings : (string * imm) list
