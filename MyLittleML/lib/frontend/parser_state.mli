[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type 'a t

val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
val ( *> ) : 'a t -> 'b t -> 'b t
val ( <* ) : 'a t -> 'b t -> 'a t
val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
val ( <|> ) : 'a t -> 'a t -> 'a t
val tracked : 'a t -> 'a t
val fail_at : string -> 'a t
val peek_char : char option t
val advance : int -> unit t
val satisfy : (char -> bool) -> char t
val char : char -> char t
val string : string -> string t
val take_while1 : (char -> bool) -> string t
val fix : ('a t -> 'a t) -> 'a t
val many : 'a t -> 'a list t
val many1 : 'a t -> 'a list t
val skip_many : 'a t -> unit t
val option : 'a -> 'a t -> 'a t
val choice : 'a t list -> 'a t
val run : 'a t -> string -> ('a, string) result
