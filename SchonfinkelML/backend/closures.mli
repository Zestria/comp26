[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Anf = SchonfinkelML_middleend.Anf

(** How an application [head a1 .. an] has to be compiled. *)
type call =
  | Direct of string (** known function, all arguments given: plain [call] *)
  | Partial of string * int
  (** known function of the given arity, fewer arguments: build a closure *)
  | Over of string * int (** known function, too many arguments *)
  | Unknown (** the callee is a value (parameter, result of a call): apply a closure *)

(** [classify ~arity_of head nargs] decides how to compile the application.
    [arity_of f] must return the arity of [f] when [f] is a known top-level
    function and [None] otherwise. ANF is not modified: the decision is a pure
    function of the callee's arity and the number of arguments. *)
val classify : arity_of:(string -> int option) -> Anf.immexpr -> int -> call
