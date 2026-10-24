[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Monad : sig
  type 'a t = Common.instr list -> ('a * Common.instr list, Common.error) result

  val return : 'a -> 'b -> ('a * 'b, 'c) result

  val bind
    :  ('a -> ('b * 'c, 'd) result)
    -> ('b -> 'c -> ('e, 'd) result)
    -> 'a
    -> ('e, 'd) result

  val ( let* )
    :  ('a -> ('b * 'c, 'd) result)
    -> ('b -> 'c -> ('e, 'd) result)
    -> 'a
    -> ('e, 'd) result

  val emit : Common.instr Common.checked -> unit t
  val run : unit t -> (Common.instr list, Common.error) result
end
