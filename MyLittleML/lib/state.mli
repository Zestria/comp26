[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type ('s, 'a) t = 's -> 'a * 's

val return : 'a -> ('s, 'a) t
val bind : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val ( let* ) : ('s, 'a) t -> ('a -> ('s, 'b) t) -> ('s, 'b) t
val get : ('s, 's) t
val put : 's -> ('s, unit) t
val modify : ('s -> 's) -> ('s, unit) t
val eval : ('s, 'a) t -> 's -> 'a
val map_list : ('a -> ('s, 'b) t) -> 'a list -> ('s, 'b list) t
