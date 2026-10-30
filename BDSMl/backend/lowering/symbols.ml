[@@@ocaml.text "/*"]

(** Copyright 2026, BDSMl contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
module Names = Map.Make (String)

type function_info =
  { symbol : string
  ; arity : int
  }

type binding =
  | Function of function_info
  | Global of string

type t =
  { bindings : binding Names.t
  ; global_symbols : string list
  }

exception Error of string

let create program =
  let builtins =
    Names.singleton
      "print_int"
      (Function { symbol = Abi.Runtime.print_int; arity = Abi.Runtime.print_int_arity })
  in
  let bindings, reversed_globals, _ =
    List.fold_left
      (fun (bindings, globals, index) -> function
         | ADecl (_, PVar name, parameters, _) ->
           if Names.mem name bindings
           then raise (Error ("RISC-V: duplicate top-level name: " ^ name));
           let binding, globals =
             match parameters with
             | [] ->
               let symbol = Printf.sprintf "bd_global_%d" index in
               Global symbol, symbol :: globals
             | _ ->
               ( Function
                   { symbol = Printf.sprintf "bd_fn_%d" index
                   ; arity = List.length parameters
                   }
               , globals )
           in
           Names.add name binding bindings, globals, index + 1
         | ADecl (_, (PUnit | PUnderscore), _ :: _, _) ->
           raise (Error "RISC-V: a function definition must bind a name")
         | ADecl (_, (PUnit | PUnderscore), [], _) | AEval _ -> bindings, globals, index)
      (builtins, [], 0)
      program
  in
  { bindings; global_symbols = List.rev reversed_globals }
;;

let find symbols name = Names.find_opt name symbols.bindings

let require symbols name =
  match find symbols name with
  | Some binding -> binding
  | None -> raise (Error ("RISC-V: unknown identifier: " ^ name))
;;

let globals symbols = symbols.global_symbols
