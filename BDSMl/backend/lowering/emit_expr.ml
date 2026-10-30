[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame

let binary context environment operator left right =
  let right_allocates = Emit_call.allocates context environment right in
  let* () = Emit_call.value context environment T0 left in
  let* () =
    if right_allocates
    then store T0 ~base:Sp ~offset:(Frame.operand_slot context.frame)
    else return ()
  in
  let* () = Emit_call.value context environment T1 right in
  let* () =
    if right_allocates
    then load T0 ~base:Sp ~offset:(Frame.operand_slot context.frame)
    else return ()
  in
  let emit_op = instruction in
  match operator with
  | Plus -> emit_op (Binary (Add, A0, T0, T1))
  | Minus -> emit_op (Binary (Sub, A0, T0, T1))
  | Mult -> emit_op (Binary (Mul, A0, T0, T1))
  | Div ->
    let* nonzero = fresh_label in
    let* () = emit_op (Bnez (T1, nonzero)) in
    let* () = emit_op (Call Abi.Runtime.division_by_zero) in
    let* () = emit (Label nonzero) in
    emit_op (Binary (Div, A0, T0, T1))
  | Equal | NotEqual ->
    let* () = emit_op (Binary (Xor, A0, T0, T1)) in
    emit_op (Unary ((if operator = Equal then Seqz else Snez), A0, A0))
  | Less -> emit_op (Binary (Slt, A0, T0, T1))
  | More -> emit_op (Binary (Slt, A0, T1, T0))
  | ELess ->
    let* () = emit_op (Binary (Slt, A0, T1, T0)) in
    emit_op (Xori (A0, A0, 1))
  | EMore ->
    let* () = emit_op (Binary (Slt, A0, T0, T1)) in
    emit_op (Xori (A0, A0, 1))
  | And | Or ->
    raise (Symbols.Error "RISC-V: ANF must lower short-circuit operators to CIf")
;;

let rec expression context environment = function
  | ACExpr computation -> emit_computation context environment computation
  | ALet (pattern, computation, body) ->
    let* () = emit_computation context environment computation in
    let* environment =
      match pattern with
      | PUnit | PUnderscore -> return environment
      | PVar name ->
        let offset = Frame.slot context.Emit_call.frame name in
        let* () = store A0 ~base:Sp ~offset in
        return ((name, offset) :: environment)
    in
    expression context environment body

and emit_computation context environment = function
  | CImm immediate -> Emit_call.value context environment A0 immediate
  | CApp (function_, arguments) -> Emit_call.call context environment function_ arguments
  | CBinOp (operator, left, right) -> binary context environment operator left right
  | CUnOp (operator, operand) ->
    let* () = Emit_call.value context environment A0 operand in
    instruction
      (Unary
         ( (match operator with
            | Neg -> Neg
            | Not -> Seqz)
         , A0
         , A0 ))
  | CIf (condition, if_true, if_false) ->
    let* if_true_label = fresh_label in
    let* if_false_label = fresh_label in
    let* end_label = fresh_label in
    let* () = Emit_call.value context environment A0 condition in
    (* Keep conditional branches short, independent of branch body size. *)
    let* () = instruction (Bnez (A0, if_true_label)) in
    let* () = instruction (Jump if_false_label) in
    let* () = emit (Label if_true_label) in
    let* () = expression context environment if_true in
    let* () = instruction (Jump end_label) in
    let* () = emit (Label if_false_label) in
    let* () = expression context environment if_false in
    emit (Label end_label)
;;
