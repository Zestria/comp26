[@@@ocaml.text "Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors"]

[@@@ocaml.text "SPDX-License-Identifier: LGPL-3.0-or-later"]

open Shmel_middleend
open Riscv
module Env = Map.Make (String)

let ( let* ) = Result.bind

let load_global globals rd name =
  match Env.find_opt name globals with
  | Some label -> Ok [ La (rd, label); Ld (rd, 0, rd) ]
  | None -> Error ("unbound variable " ^ name)
;;

let store_global label = [ La (T 0, label); Sd (A 0, 0, T 0) ]
let slot label = [ Label label; Directive ".zero 8" ]

let make_closure code arity =
  let* closure = Closure.create ~code ~arity in
  Ok
    [ La (A 0, Closure.code closure)
    ; Li (A 1, Closure.arity closure)
    ; Call "shmel_make_closure"
    ]
;;

let compile program =
  let builtin = "shmel_builtin_print_int" in
  let* print = make_closure "shmel_print_int" 1 in
  let rec declarations index globals functions slots initializers = function
    | [] ->
      Ok
        ([ Directive ".text" ]
         @ List.concat (List.rev functions)
         @ [ Directive ".globl main"
           ; Directive ".type main, @function"
           ; Label "main"
           ; I (Addi, Sp, Sp, -16)
           ; Sd (Ra, 8, Sp)
           ]
         @ List.concat (List.rev initializers)
         @ [ Li (A 0, 0)
           ; Ld (Ra, 8, Sp)
           ; I (Addi, Sp, Sp, 16)
           ; Ret
           ; Directive ".size main, .-main"
           ; Directive ".bss"
           ; Directive ".balign 8"
           ]
         @ List.concat (List.rev slots)
         @ [ Directive ".section .note.GNU-stack,\"\",@progbits" ])
    | declaration :: rest ->
      let code = Printf.sprintf "shmel_fn_%d" index in
      let global = Printf.sprintf "shmel_global_%d" index in
      let name, params, body, recursive =
        match declaration with
        | Anf.DLet (name, params, body) -> Some name, params, body, false
        | Anf.DLetRec (name, params, body) -> Some name, params, body, true
        | Anf.DExpr body -> None, [], body, false
      in
      if recursive && List.is_empty params
      then Error "recursive global values are not supported"
      else (
        let body_globals =
          match name, recursive with
          | Some name, true -> Env.add name global globals
          | _ -> globals
        in
        let hooks = Calls.hooks ~load_global:(load_global body_globals) in
        let* function_code, initialize =
          match params, body with
          | [], Anf.AExpr (Anf.CImm (Anf.ImmInt value)) -> Ok ([], [ Li (A 0, value) ])
          | [], Anf.AExpr (Anf.CImm (Anf.ImmVar name)) ->
            Result.map (fun code -> [], code) (load_global body_globals (A 0) name)
          | _ ->
            let* instructions = Codegen.compile_function hooks ~name:code ~params body in
            let* initialize =
              if List.is_empty params
              then Ok [ Call code ]
              else make_closure code (List.length params)
            in
            Ok
              ( (Directive (".type " ^ code ^ ", @function") :: instructions)
                @ [ Directive (".size " ^ code ^ ", .-" ^ code) ]
              , initialize )
        in
        let globals, slots, initialize =
          match name with
          | None -> globals, slots, initialize
          | Some name ->
            ( Env.add name global globals
            , slot global :: slots
            , initialize @ store_global global )
        in
        declarations
          (index + 1)
          globals
          (function_code :: functions)
          slots
          (initialize :: initializers)
          rest)
  in
  declarations
    0
    (Env.singleton "print_int" builtin)
    []
    [ slot builtin ]
    [ print @ store_global builtin ]
    program
;;
