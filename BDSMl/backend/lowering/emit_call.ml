[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame

type environment = (string * int) list

type context =
  { symbols : Symbols.t
  ; frame : Frame.t
  }

let allocates context environment = function
  | ImmId name when not (List.mem_assoc name environment) ->
    (match Symbols.find context.symbols name with
     | Some (Symbols.Function _) -> true
     | Some (Symbols.Global _) | None -> false)
  | ImmId _ | ImmInt _ | ImmBool _ | ImmUnit -> false
;;

let value context environment destination immediate =
  match immediate with
  | ImmInt number -> instruction (Li (destination, Int64.of_int number))
  | ImmBool boolean ->
    instruction (Li (destination, if boolean then Abi.true_value else Abi.false_value))
  | ImmUnit -> instruction (Li (destination, Abi.unit_value))
  | ImmId name ->
    (match List.assoc_opt name environment with
     | Some offset -> load destination ~base:Sp ~offset
     | None ->
       (match Symbols.require context.symbols name with
        | Symbols.Global symbol ->
          let* () = instruction (La (T5, symbol)) in
          load destination ~base:T5 ~offset:0
        | Symbols.Function function_ ->
          let* () = instruction (La (A0, function_.symbol)) in
          let* () = instruction (Li (A1, Int64.of_int function_.arity)) in
          let* () = instruction (Call Abi.Runtime.make_closure) in
          if destination <> A0 then instruction (Mv (destination, A0)) else return ()))
;;

let direct_target context environment function_ count =
  match function_ with
  | ImmId name when not (List.mem_assoc name environment) ->
    (match Symbols.require context.symbols name with
     | Symbols.Function function_ when function_.arity = count -> Some function_.symbol
     | Symbols.Function _ | Symbols.Global _ -> None)
  | ImmId _ -> None
  | ImmInt _ | ImmBool _ | ImmUnit ->
    raise (Symbols.Error "RISC-V: a literal cannot be applied as a function")
;;

let call context environment function_ arguments =
  let count = List.length arguments in
  let target = direct_target context environment function_ count in
  match target with
  | Some symbol when not (List.exists (allocates context environment) arguments) ->
    (* No argument load calls runtime, so registers can be filled directly. *)
    let* () =
      iteri
        (fun index argument ->
           if index < Abi.register_argument_count
           then value context environment (List.nth argument_registers index) argument
           else
             let* () = value context environment T0 argument in
             store
               T0
               ~base:Sp
               ~offset:(Frame.argument_offset (index - Abi.register_argument_count)))
        arguments
    in
    instruction (Call symbol)
  | _ ->
    let* () =
      match target with
      | Some _ -> return ()
      | None ->
        let* () = value context environment A0 function_ in
        store A0 ~base:Sp ~offset:(Frame.callee_slot context.frame)
    in
    let* () =
      iteri
        (fun index argument ->
           let* () = value context environment A0 argument in
           store A0 ~base:Sp ~offset:(Frame.argument_offset index))
        arguments
    in
    (match target with
     | Some symbol ->
       (* Load registers before moving high arguments down into the outgoing
          stack area. Sources and destinations can overlap. *)
       let* () =
         iteri
           (fun index register ->
              if index < count
              then load register ~base:Sp ~offset:(Frame.argument_offset index)
              else return ())
           argument_registers
       in
       let* () =
         iteri
           (fun index _ ->
              if index >= Abi.register_argument_count
              then
                let* () = load T0 ~base:Sp ~offset:(Frame.argument_offset index) in
                store
                  T0
                  ~base:Sp
                  ~offset:(Frame.argument_offset (index - Abi.register_argument_count))
              else return ())
           arguments
       in
       instruction (Call symbol)
     | None ->
       let* () = load A0 ~base:Sp ~offset:(Frame.callee_slot context.frame) in
       let* () = instruction (Li (A1, Int64.of_int count)) in
       let* () = address A2 ~base:Sp ~offset:0 in
       instruction (Call Abi.Runtime.apply))
;;
