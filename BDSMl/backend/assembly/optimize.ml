[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Asm

(* Only adjacent instructions are forwarded: calls, labels and branches
   terminate the opportunity, so no control-flow or alias assumptions leak. *)
let rec adjacent = function
  | Instruction (Mv (destination, source))
    :: Instruction (Mv (back, value)) :: rest
    when back = source && value = destination ->
    adjacent (Instruction (Mv (destination, source)) :: rest)
  | Instruction (Sd (source, offset, base))
    :: Instruction (Ld (destination, other_offset, other_base))
    :: rest
    when offset = other_offset && base = other_base ->
    Instruction (Sd (source, offset, base))
    :: adjacent (Instruction (Mv (destination, source)) :: rest)
  | Instruction (Mv (destination, source)) :: rest when destination = source ->
    adjacent rest
  | Instruction (Addi (destination, source, 0)) :: rest ->
    adjacent (Instruction (Mv (destination, source)) :: rest)
  | Instruction (Jump target) :: Label next :: rest when target = next ->
    adjacent (Label next :: rest)
  | line :: rest -> line :: adjacent rest
  | [] -> []
;;

let leaf_function lines =
  if
    List.exists
      (function
        | Instruction (Call _) -> true
        | _ -> false)
      lines
  then lines
  else
    List.filter
      (function
        | Instruction (Sd (Ra, _, Sp) | Ld (Ra, _, Sp)) -> false
        | _ -> true)
      lines
;;

let rec functions = function
  | Directive (Function name) :: rest ->
    let rec collect reversed = function
      | (Directive (End_function symbol) as end_) :: rest when symbol = name ->
        List.rev (end_ :: reversed), rest
      | line :: rest -> collect (line :: reversed) rest
      | [] -> List.rev reversed, []
    in
    let body, rest = collect [] rest in
    Directive (Function name) :: (leaf_function body @ functions rest)
  | line :: rest -> line :: functions rest
  | [] -> []
;;

let run program = functions program |> adjacent
