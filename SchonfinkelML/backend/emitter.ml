[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Common

type item =
  | Instruction of instr
  | Label of string

module Monad = struct
  type 'a t = item list -> ('a * item list, error) result

  let return (x : 'a) : 'a t = fun code -> Ok (x, code)

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
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
    | Ok instruction -> Ok ((), Instruction instruction :: code)
  ;;

  let label (name : string) : unit t =
    fun code ->
    if String.trim name = ""
    then Error (Invalid_label name)
    else Ok ((), Label name :: code)
  ;;

  let start : unit t = label "_start"
  let function_label (name : string) : unit t = label name

  let run (program : unit t) : (item list, error) result =
    match program [] with
    | Error e -> Error e
    | Ok ((), reversed_code) -> Ok (List.rev reversed_code)
  ;;
end
