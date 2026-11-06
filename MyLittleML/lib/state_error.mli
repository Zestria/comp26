[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type ('s, 'a, 'e) t = 's -> ('a * 's, 'e * 's) result

val return : 'a -> ('s, 'a, 'e) t
val fail : 'e -> ('s, 'a, 'e) t
val bind : ('s, 'a, 'e) t -> ('a -> ('s, 'b, 'e) t) -> ('s, 'b, 'e) t
val ( let* ) : ('s, 'a, 'e) t -> ('a -> ('s, 'b, 'e) t) -> ('s, 'b, 'e) t
val get : ('s, 's, 'e) t
val put : 's -> ('s, unit, 'e) t
val modify : ('s -> 's) -> ('s, unit, 'e) t
val of_result : ('a, 'e) result -> ('s, 'a, 'e) t
val eval : ('s, 'a, 'e) t -> 's -> ('a, 'e) result
val map_list : ('a -> ('s, 'b, 'e) t) -> 'a list -> ('s, 'b list, 'e) t
