[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** A-normal form: every argument of an operation is an immediate
    (a constant or a variable), and every intermediate result is named. *)

type imm =
  | ImmInt of int
  | ImmVar of string

(** The condition of [if]. A comparison is kept in the condition rather than
    bound to a name, since RISC-V and LLVM can compare and branch at once. *)
type cond =
  | CondImm of imm
  | CondCmp of Shmel_lib.Ast.binop * imm * imm (** only [=], [<], [>], [<=], [>=] *)

type cexpr =
  | CImm of imm
  | CNeg of imm
  | CBinOp of Shmel_lib.Ast.binop * imm * imm
  | CApp of imm * imm list (** [f a1 ... an], [n >= 1] *)
  | CIf of cond * aexpr * aexpr
  | CFun of string list * aexpr (** [fun p1 ... pn -> body], [n >= 1] *)

and aexpr =
  | ALet of string * cexpr * aexpr
  | ALetRec of string * string list * aexpr * aexpr
  (** [let rec f p1 ... pn = body in rest], [n >= 1] *)
  | AExpr of cexpr

(** A top-level declaration. *)
type decl =
  | DLet of string * string list * aexpr (** [let f p1 ... pn = body], [n >= 0] *)
  | DLetRec of string * string list * aexpr
  (** [let rec f p1 ... pn = body], [n >= 1] *)
  | DExpr of aexpr (** an expression at the beginning of the program *)

type program = decl list

(** Converts an expression to ANF. Binders are renamed only when a name is
    already taken (to [x_1], [x_2], ...), temporaries are named [t0], [t1], ...
    To keep the result small, immediates are never bound to a name, tail calls
    are not bound, [f a b] becomes one application, [let x = <immediate>] is
    substituted, integer arithmetic on constants is folded, [fun x -> f x] is
    reduced to [f], [- - e] to [e], and unused pure bindings are dropped.
    Comparisons stay in [if] conditions ([let x = a < b in if x] becomes
    [if a < b]), and [if] on a comparison of constants keeps one branch.
    The result computes the same value as the input (see {!Eval}). *)
val of_expr : Shmel_lib.Ast.expr -> aexpr

(** Converts ANF back to an ordinary expression (e.g. to run it with {!Eval}). *)
val to_expr : aexpr -> Shmel_lib.Ast.expr

(** Prints the program in the source syntax, so it can be parsed again. *)
val pp : Format.formatter -> aexpr -> unit

(** Converts every declaration of a program with {!of_expr}. Declared names are
    visible to the rest of the program, so they are never renamed, and
    declarations are never dropped. *)
val of_program : Shmel_lib.Ast.program -> program

(** Converts ANF back to an ordinary program. *)
val to_program : program -> Shmel_lib.Ast.program

(** Prints the program in the source syntax, declarations are separated by an
    empty line. *)
val pp_program : Format.formatter -> program -> unit
