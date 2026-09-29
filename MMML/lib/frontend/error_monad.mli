[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Result computations carrying a positioned frontend diagnostic. *)

type error =
  { location : Location.point
  ; message : string
  }

type 'a t = ('a, error) result

val return : 'a -> 'a t
val error : Location.point -> string -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val map : 'a t -> ('a -> 'b) -> 'b t

module Syntax : sig
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
end

val pp_error : Format.formatter -> error -> unit
val show_error : error -> string
