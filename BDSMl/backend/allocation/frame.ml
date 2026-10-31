[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Names = Map.Make (String)

type location =
  | Register of register
  | Stack of int
  | Unused

type t =
  { bytes : int
  ; locations : location Names.t
  ; call_words : int
  ; saved : (register * int) list
  ; spills : int
  }

let argument_offset index = index * Abi.word_size

let rec collect_expr (names, max_arguments) = function
  | ACExpr expression -> collect_computation (names, max_arguments) expression
  | ALet (pattern, expression, body) ->
    let names =
      match pattern with
      | PVar name -> Names.add name () names
      | PUnit | PUnderscore -> names
    in
    collect_expr (collect_computation (names, max_arguments) expression) body

and collect_computation (names, max_arguments) = function
  | CApp (_, arguments) -> names, max max_arguments (List.length arguments)
  | CIf (_, if_true, if_false) ->
    collect_expr (collect_expr (names, max_arguments) if_true) if_false
  | CImm _ | CBinOp _ | CUnOp _ -> names, max_arguments
;;

let create parameters bodies =
  let names =
    List.fold_left
      (fun names -> function
         | PVar name -> Names.add name () names
         | PUnit | PUnderscore -> names)
      Names.empty
      parameters
  in
  let names, call_words = List.fold_left collect_expr (names, 0) bodies in
  let assigned =
    Linear_scan.allocate
      ~registers:Linear_scan.registers
      (Linear_scan.intervals parameters bodies)
  in
  let spills =
    List.fold_left
      (fun count (_, assignment) ->
         match assignment with
         | Linear_scan.Register _ -> count
         | Linear_scan.Spill index -> max count (index + 1))
      0
      assigned
  in
  let first_slot = (call_words + 2) * Abi.word_size in
  let locations = Names.map (fun () -> Unused) names in
  let locations =
    List.fold_left
      (fun locations (name, assignment) ->
        let location =
          match assignment with
          | Linear_scan.Register register -> Register register
          | Linear_scan.Spill index -> Stack (first_slot + index * Abi.word_size)
        in
        Names.add name location locations)
      locations
      assigned
  in
  let used =
    List.filter
      (fun register ->
         List.exists
           (fun (_, assignment) -> assignment = Linear_scan.Register register)
           assigned)
      Linear_scan.registers
  in
  let first_save = first_slot + spills * Abi.word_size in
  let saved =
    List.mapi (fun index register -> register, first_save + index * Abi.word_size) used
  in
  let end_slot = first_save + List.length saved * Abi.word_size in
  let bytes = end_slot + Abi.word_size in
  let bytes =
    (bytes + Abi.stack_alignment - 1) / Abi.stack_alignment * Abi.stack_alignment
  in
  { bytes; locations; call_words; saved; spills }
;;

let size frame = frame.bytes
let callee_slot frame = frame.call_words * Abi.word_size
let operand_slot frame = (frame.call_words + 1) * Abi.word_size

let location frame name =
  match Names.find_opt name frame.locations with
  | Some location -> location
  | None -> invalid_arg ("Frame.location: unallocated name " ^ name)
;;

let slot frame name =
  match location frame name with
  | Stack offset -> offset
  | Register _ | Unused -> invalid_arg ("Frame.slot: binding is not spilled: " ^ name)
;;

let saved_registers frame = List.map fst frame.saved
let spill_count frame = frame.spills

let read location destination =
  match location with
  | Register source ->
    if source = destination then return () else instruction (Mv (destination, source))
  | Stack offset -> load destination ~base:Sp ~offset
  | Unused -> invalid_arg "Frame.read: reference to an unused binding"
;;

let write location source =
  match location with
  | Register destination ->
    if source = destination then return () else instruction (Mv (destination, source))
  | Stack offset -> store source ~base:Sp ~offset
  | Unused -> return ()
;;

let prologue frame parameters =
  let* () = adjust_stack (-frame.bytes) in
  let* () = store Ra ~base:Sp ~offset:(frame.bytes - Abi.word_size) in
  let* () = iter (fun (register, offset) -> store register ~base:Sp ~offset) frame.saved in
  iteri
    (fun index -> function
       | PUnit | PUnderscore -> return ()
       | PVar name ->
         let destination = location frame name in
         match destination with
         | Unused -> return ()
         | Register _ | Stack _ when index < Abi.register_argument_count ->
           write destination (List.nth argument_registers index)
         | Register _ | Stack _ ->
           let* () =
             load
               T0
               ~base:Sp
               ~offset:
                 (frame.bytes + argument_offset (index - Abi.register_argument_count))
           in
           write destination T0)
    parameters
;;

let epilogue frame =
  let* () = iter (fun (register, offset) -> load register ~base:Sp ~offset) frame.saved in
  let* () = load Ra ~base:Sp ~offset:(frame.bytes - Abi.word_size) in
  let* () = adjust_stack frame.bytes in
  instruction Ret
;;
