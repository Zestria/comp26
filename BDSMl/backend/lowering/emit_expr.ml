[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame

let binary context environment destination operator left right =
  let right_allocates = Emit_call.allocates context environment right in
  let* left_register = Emit_call.operand context environment T0 left in
  let* () =
    if right_allocates
    then store left_register ~base:Sp ~offset:(Frame.operand_slot context.frame)
    else return ()
  in
  let* right_register = Emit_call.operand context environment T1 right in
  let* left_register =
    if right_allocates
    then
      let* () = load T0 ~base:Sp ~offset:(Frame.operand_slot context.frame) in
      return T0
    else return left_register
  in
  let emit_op = instruction in
  match operator with
  | Plus -> emit_op (Binary (Add, destination, left_register, right_register))
  | Minus -> emit_op (Binary (Sub, destination, left_register, right_register))
  | Mult -> emit_op (Binary (Mul, destination, left_register, right_register))
  | Div ->
    let* nonzero = fresh_label in
    let* () = emit_op (Bnez (right_register, nonzero)) in
    let* () = emit_op (Call Abi.Runtime.division_by_zero) in
    let* () = emit (Label nonzero) in
    emit_op (Binary (Div, destination, left_register, right_register))
  | Equal | NotEqual ->
    let* () = emit_op (Binary (Xor, destination, left_register, right_register)) in
    emit_op (Unary ((if operator = Equal then Seqz else Snez), destination, destination))
  | Less -> emit_op (Binary (Slt, destination, left_register, right_register))
  | More -> emit_op (Binary (Slt, destination, right_register, left_register))
  | ELess ->
    let* () = emit_op (Binary (Slt, destination, right_register, left_register)) in
    emit_op (Xori (destination, destination, 1))
  | EMore ->
    let* () = emit_op (Binary (Slt, destination, left_register, right_register)) in
    emit_op (Xori (destination, destination, 1))
  | And | Or ->
    raise (Symbols.Error "RISC-V: ANF must lower short-circuit operators to CIf")
;;

let rec expression ?(destination = A0) context environment = function
  | ACExpr computation -> emit_computation context environment destination computation
  | ALet (pattern, computation, body) ->
    let binding_destination = match pattern with
      | PVar name -> (match Frame.location context.Emit_call.frame name with
          | Frame.Register register -> register
          | Frame.Stack _ | Frame.Unused -> A0)
      | PUnit | PUnderscore -> A0
    in
    let* () = emit_computation context environment binding_destination computation in
    let* environment =
      match pattern with
      | PUnit | PUnderscore -> return environment
      | PVar name ->
        let location = Frame.location context.Emit_call.frame name in
        let* () = Frame.write location binding_destination in
        return ((name, location) :: environment)
    in
    expression ~destination context environment body

and emit_computation context environment destination = function
  | CImm immediate -> Emit_call.value context environment destination immediate
  | CApp (function_, arguments) ->
    let* () = Emit_call.call context environment function_ arguments in
    instruction (Mv (destination, A0))
  | CBinOp (operator, left, right) -> binary context environment destination operator left right
  | CUnOp (operator, operand) ->
    let* source = Emit_call.operand context environment T0 operand in
    instruction
      (Unary
         ( (match operator with
            | Neg -> Neg
            | Not -> Seqz)
         , destination
         , source ))
  | CIf (condition, if_true, if_false) ->
    let* if_true_label = fresh_label in
    let* if_false_label = fresh_label in
    let* end_label = fresh_label in
    let* () = Emit_call.value context environment A0 condition in
    (* Keep conditional branches short, independent of branch body size. *)
    let* () = instruction (Bnez (A0, if_true_label)) in
    let* () = instruction (Jump if_false_label) in
    let* () = emit (Label if_true_label) in
    let* () = expression ~destination context environment if_true in
    let* () = instruction (Jump end_label) in
    let* () = emit (Label if_false_label) in
    let* () = expression ~destination context environment if_false in
    emit (Label end_label)
;;
