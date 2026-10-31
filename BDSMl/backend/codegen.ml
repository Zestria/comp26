[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
open Assembly.Asm
module Frame = Allocation.Frame
module Symbols = Lowering.Symbols
module Emit_expr = Lowering.Emit_expr

let start_function symbol =
  let* () = emit (Directive (Align 2)) in
  let* () = emit (Directive (Function symbol)) in
  emit (Label symbol)
;;

let emit_function symbols name parameters body =
  let symbol =
    match Symbols.require symbols name with
    | Symbols.Function function_ -> function_.symbol
    | Symbols.Global _ -> raise (Symbols.Error ("RISC-V: expected a function: " ^ name))
  in
  let frame = Frame.create parameters [ body ] in
  let environment =
    List.filter_map
      (function
        | PVar name -> Some (name, Frame.location frame name)
        | PUnit | PUnderscore -> None)
      parameters
  in
  let* () = start_function symbol in
  let* () = Frame.prologue frame parameters in
  let* () = Emit_expr.expression { symbols; frame } environment body in
  let* () = Frame.epilogue frame in
  emit (Directive (End_function symbol))
;;

let initializers program =
  List.filter_map
    (function
      | ADecl (_, _, [], body) | AEval body -> Some body
      | ADecl (_, _, _ :: _, _) -> None)
    program
;;

let emit_main symbols program =
  let frame = Frame.create [] (initializers program) in
  let* () = emit (Directive (Global "main")) in
  let* () = start_function "main" in
  let* () = Frame.prologue frame [] in
  let context = Lowering.Emit_call.{ symbols; frame } in
  let* () =
    iter
      (function
        | AEval body -> Emit_expr.expression context [] body
        | ADecl (_, pattern, [], body) ->
          let* () = Emit_expr.expression context [] body in
          (match pattern with
           | PUnit | PUnderscore -> return ()
           | PVar name ->
             (match Symbols.require symbols name with
              | Symbols.Global symbol ->
                let* () = instruction (La (T5, symbol)) in
                store A0 ~base:T5 ~offset:0
              | Symbols.Function _ ->
                raise (Symbols.Error ("RISC-V: expected a global value: " ^ name))))
        | ADecl (_, _, _ :: _, _) -> return ())
      program
  in
  let* () = instruction (Li (A0, 0L)) in
  let* () = Frame.epilogue frame in
  emit (Directive (End_function "main"))
;;

let generate program =
  try
    let symbols = Symbols.create program in
    let output =
      let* () =
        match Symbols.globals symbols with
        | [] -> return ()
        | globals ->
          let* () = emit (Directive Bss) in
          iter
            (fun symbol ->
               let* () = emit (Directive (Align 3)) in
               let* () = emit (Label symbol) in
               emit (Directive (Zero_bytes Abi.word_size)))
            globals
      in
      let* () = emit (Directive Text) in
      let* () =
        iter
          (function
            | ADecl (_, PVar name, (_ :: _ as parameters), body) ->
              emit_function symbols name parameters body
            | ADecl _ | AEval _ -> return ())
          program
      in
      let* () = emit_main symbols program in
      emit (Directive Non_executable_stack)
    in
    Ok (Assembly.Asm_printer.print (Assembly.Optimize.run (finish output)))
  with
  | Symbols.Error message -> Error message
;;
