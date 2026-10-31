[@@@ocaml.text "/*"]

(** Copyright 2026, shrexer06 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type ('function_, 'callee) printers =
  { identifier : Format.formatter -> Id.t -> unit
  ; atom : Format.formatter -> Anf.atom -> unit
  ; term : Format.formatter -> ('function_, 'callee) Anf.term -> unit
  }

(** Immutable names allocated in first-occurrence order within one dump. *)
module Names : sig
  type t

  val empty : t
  val add : t -> Id.t -> t
  val add_all : t -> Id.t list -> t
  val atom : t -> Anf.atom -> t
end

(** Collect ordinary terms' names in printing order. The callback collects
    names in the stage-specific function payload. *)
val names
  :  callee:(Names.t -> 'callee -> arity:int -> Names.t)
  -> function_:(Names.t -> 'function_ -> Names.t)
  -> Names.t
  -> ('function_, 'callee) Anf.term
  -> Names.t

(** Build printers sharing a complete name table. The function callback gets
    the identifier, atom and term printers for its payload. All Ids to be
    printed must have been collected beforehand. *)
val make
  :  Names.t
  -> callee:
       ((Format.formatter -> Id.t -> unit)
        -> (Format.formatter -> Anf.atom -> unit)
        -> Format.formatter
        -> 'callee
        -> unit)
  -> ((Format.formatter -> Id.t -> unit)
      -> (Format.formatter -> Anf.atom -> unit)
      -> (Format.formatter -> ('function_, 'callee) Anf.term -> unit)
      -> Format.formatter
      -> 'function_
      -> unit)
  -> ('function_, 'callee) printers

val separated
  :  string
  -> (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a list
  -> unit

val expression : Format.formatter -> Anf.expression -> unit
val program : Format.formatter -> Anf.program -> unit
