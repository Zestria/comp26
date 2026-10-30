[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type register =
  | Zero
  | Ra
  | Sp
  | Gp
  | Tp
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | S8
  | S9
  | S10
  | S11
  | A0
  | A1
  | A2
  | A3
  | A4
  | A5
  | A6
  | A7

type binary =
  | Add
  | Sub
  | Mul
  | Div
  | Xor
  | Slt
  | And
  | Or

type unary =
  | Neg
  | Seqz
  | Snez

type instruction =
  | Li of register * int64
  | La of register * string
  | Mv of register * register
  | Binary of binary * register * register * register
  | Unary of unary * register * register
  | Addi of register * register * int
  | Xori of register * register * int
  | Ld of register * int * register
  | Sd of register * int * register
  | Beqz of register * string
  | Bnez of register * string
  | Jump of string
  | Call of string
  | Ret

type directive =
  | Text
  | Bss
  | Global of string
  | Align of int
  | Function of string
  | End_function of string
  | Zero_bytes of int
  | Non_executable_stack

type line =
  | Instruction of instruction
  | Directive of directive
  | Label of string

type program = line list

let argument_registers = [ A0; A1; A2; A3; A4; A5; A6; A7 ]

type state =
  { reversed : line list
  ; next_label : int
  }

type 'a code = state -> 'a * state

let return value state = value, state

let bind code next state =
  let value, state = code state in
  next value state
;;

let ( let* ) = bind
let emit line state = (), { state with reversed = line :: state.reversed }
let instruction op = emit (Instruction op)

let fresh_label state =
  ( Printf.sprintf ".Lbd_%d" state.next_label
  , { state with next_label = state.next_label + 1 } )
;;

let finish code =
  let state = snd (code { reversed = []; next_label = 0 }) in
  List.rev state.reversed
;;

let rec iter f = function
  | [] -> return ()
  | item :: rest ->
    let* () = f item in
    iter f rest
;;

let iteri f items =
  iter (fun (index, item) -> f index item) (List.mapi (fun i item -> i, item) items)
;;

let fits_immediate offset = offset >= -2048 && offset <= 2047

let check_scratch data base =
  if data = T6 || base = T6 then invalid_arg "Asm: T6 is reserved for address expansion"
;;

let address destination ~base ~offset =
  check_scratch destination base;
  if fits_immediate offset
  then instruction (Addi (destination, base, offset))
  else
    let* () = instruction (Li (T6, Int64.of_int offset)) in
    instruction (Binary (Add, destination, base, T6))
;;

let load destination ~base ~offset =
  check_scratch destination base;
  if fits_immediate offset
  then instruction (Ld (destination, offset, base))
  else
    let* () = instruction (Li (T6, Int64.of_int offset)) in
    let* () = instruction (Binary (Add, T6, base, T6)) in
    instruction (Ld (destination, 0, T6))
;;

let store source ~base ~offset =
  check_scratch source base;
  if fits_immediate offset
  then instruction (Sd (source, offset, base))
  else
    let* () = instruction (Li (T6, Int64.of_int offset)) in
    let* () = instruction (Binary (Add, T6, base, T6)) in
    instruction (Sd (source, 0, T6))
;;

let adjust_stack amount = address Sp ~base:Sp ~offset:amount
