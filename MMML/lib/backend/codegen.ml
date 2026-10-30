[@@@ocaml.text "/*"]

(** Copyright 2026, Mikhail and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module A = Asm

let register = A.register_name

let condition = function
  | A.Eq -> "beq"
  | A.Ne -> "bne"
  | A.Lt -> "blt"
;;

let binop = function
  | A.Add -> "add"
  | A.Sub -> "sub"
  | A.Mul -> "mul"
  | A.Div -> "div"
;;

let pp_instruction formatter = function
  | A.Label label -> Format.fprintf formatter "%s:@." label
  | A.Li (destination, value) ->
    Format.fprintf formatter "\tli %s, %Ld@." (register destination) value
  | A.La (destination, label) ->
    Format.fprintf formatter "\tla %s, %s@." (register destination) label
  | A.Addi (destination, source, value) ->
    Format.fprintf
      formatter
      "\taddi %s, %s, %Ld@."
      (register destination)
      (register source)
      value
  | A.Binop (operation, destination, left, right) ->
    Format.fprintf
      formatter
      "\t%s %s, %s, %s@."
      (binop operation)
      (register destination)
      (register left)
      (register right)
  | A.Ld (destination, value, base) ->
    Format.fprintf
      formatter
      "\tld %s, %Ld(%s)@."
      (register destination)
      value
      (register base)
  | A.Sd (source, value, base) ->
    Format.fprintf formatter "\tsd %s, %Ld(%s)@." (register source) value (register base)
  | A.Branch (kind, left, right, label) ->
    Format.fprintf
      formatter
      "\t%s %s, %s, %s@."
      (condition kind)
      (register left)
      (register right)
      label
  | A.Jump label -> Format.fprintf formatter "\tj %s@." label
  | A.Call label -> Format.fprintf formatter "\tcall %s@." label
  | A.Ecall -> Format.fprintf formatter "\tecall@."
  | A.Ret -> Format.fprintf formatter "\tret@."
;;

let pp_function formatter (function_ : A.function_) =
  if function_.exported then Format.fprintf formatter "\t.globl %s@." function_.name;
  Format.fprintf formatter "%s:@." function_.name;
  List.iter (pp_instruction formatter) function_.body
;;

let pp_program formatter program = List.iter (pp_function formatter) program
let string_of_program program = Format.asprintf "%a" pp_program program
let emit_program channel program = output_string channel (string_of_program program)
