[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type token =
  | IF (** [if] *)
  | THEN (** [then] *)
  | ELSE (** [else] *)
  | LET (** [let] *)
  | REC (** [rec] *)
  | IN (** [in] *)
  | IDENT of string (** An identifier, as written in the source. *)
  | INT of int (** An integer literal. *)
  | UNIT (** The literal [()]. *)
  | PLUS (** [+] *)
  | MINUS (** [-] *)
  | TIMES (** [*] *)
  | DIV (** [/] *)
  | EQ (** [=] *)
  | LT (** [<] *)
  | GT (** [>] *)
  | LE (** [<=] *)
  | GE (** [>=] *)
  | LPAREN (** [(] *)
  | RPAREN (** [)] *)
  | EOF (** End of input. *)
  | FUN (** [fun] *)
  | RIGHTARROW (** [->] *)

type literal =
  | LitInt of int (** An integer literal. *)
  | LitUnit (** The literal [()]. *)

type let_flag =
  | Recursive (** [let rec], a recursive binding. *)
  | NotRecursive (** [let], a non-recursive binding. *)

type expr =
  | Ident of string (** A variable reference. *)
  | Const of literal (** A literal constant. *)
  | App of expr * expr (** Application [e1 e2]. *)
  | Let of let_flag * string * expr * expr (** [let x = e1 in e2]. *)
  | If of expr * expr * expr (** [if e1 then e2 else e3]. *)
  | Abstr of string * expr (** [fun x -> e]. *)

type structure_item =
  | StrLet of let_flag * string * expr (** A top-level [let] binding. *)
  | StrEval of expr (** A top-level expression to evaluate. *)

(** A whole source file: its structure items in order. *)
type program = structure_item list

(* Smart constructors, prefer them instead of ordinary ones *)

val eletrec : string -> expr -> expr -> expr
val elet : string -> expr -> expr -> expr
val econsti : int -> expr
val eunit : expr
val eapp : expr -> expr -> expr
val eif : expr -> expr -> expr -> expr
val elambda : string -> expr -> expr
val eadd : expr -> expr -> expr
val esub : expr -> expr -> expr
val emul : expr -> expr -> expr
val ediv : expr -> expr -> expr
val egte : expr -> expr -> expr
val elte : expr -> expr -> expr
val elt : expr -> expr -> expr
val egt : expr -> expr -> expr
val eeq : expr -> expr -> expr
val euminus : expr -> expr
val is_binop : string -> bool
val pp_literal : Format.formatter -> literal -> unit
val pp_token : Format.formatter -> token -> unit
val pp_expr : Format.formatter -> expr -> unit
val pp_atom_like : Format.formatter -> expr -> unit
val string_of_expr : expr -> string
val pp_structure_item : Format.formatter -> structure_item -> unit
val pp_program : Format.formatter -> program -> unit
val string_of_program : program -> string
