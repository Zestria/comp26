[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Reference interpreter. It is used to check that the middle-end passes
    (ANF, closure conversion, lambda lifting) do not change what a program
    computes. Evaluation order is left to right. *)

type builtin = Print_int (** [print_int n] prints [n] without a newline *)

type value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VBuiltin of builtin
  | VClosure of closure

and closure =
  { self : string option (** name of the function for [let rec] *)
  ; param : string
  ; body : Shmel_lib.Ast.expr
  ; env : value Map.Make(String).t
  }

type error =
  | Unbound_variable of string
  | Type_error of string
  | Division_by_zero

type outcome =
  { output : string (** what the program printed *)
  ; result : (value, error) result
    (** the value of the last declaration, [()] for an empty program *)
  }

(** Runs a program; [print_int] is predefined. *)
val run : Shmel_lib.Ast.program -> outcome

(** Evaluates an expression; its output is discarded. *)
val eval : Shmel_lib.Ast.expr -> (value, error) result

val pp_value : Format.formatter -> value -> unit
val pp_error : Format.formatter -> error -> unit
