[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common

module Monad = struct
  type 'a t = instr list -> ('a * instr list, error) result

  let return x = fun code -> Ok (x, code)

  let bind m f =
    fun code ->
    match m code with
    | Error e -> Error e
    | Ok (x, code) -> f x code
  ;;

  let ( let* ) = bind

  let emit (instruction : instr checked) : unit t =
    fun code ->
    match instruction with
    | Error e -> Error e
    | Ok instruction -> Ok ((), instruction :: code)
  ;;

  let run (program : unit t) : (instr list, error) result =
    match program [] with
    | Error e -> Error e
    | Ok ((), reversed_code) -> Ok (List.rev reversed_code)
  ;;
end
