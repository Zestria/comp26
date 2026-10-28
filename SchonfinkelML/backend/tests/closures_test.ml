[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_middleend.Anf
open SchonfinkelML_backend

let arity_of = function
  | "add" -> Some 2
  | "id" -> Some 1
  | _ -> None
;;

let%test "classify full application" =
  Closures.classify ~arity_of (ImmId "add") 2 = Closures.Direct "add"
;;

let%test "classify partial application" =
  Closures.classify ~arity_of (ImmId "add") 1 = Closures.Partial ("add", 2)
;;

let%test "classify over-application" =
  Closures.classify ~arity_of (ImmId "add") 3 = Closures.Over ("add", 2)
;;

let%test "classify unknown head (parameter)" =
  Closures.classify ~arity_of (ImmId "k") 1 = Closures.Unknown
;;

let%test "classify unknown head (immediate)" =
  Closures.classify ~arity_of (ImmNum 5) 1 = Closures.Unknown
;;

let%test "classify id full" =
  Closures.classify ~arity_of (ImmId "id") 1 = Closures.Direct "id"
;;

let%test "classify id zero args is partial" =
  Closures.classify ~arity_of (ImmId "id") 0 = Closures.Partial ("id", 1)
;;
