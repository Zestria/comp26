open BDSMl_frontend.Ast
open BDSMl_middleend.Anf.Anf_ast
module Names = Map.Make (String)

type function_info = { symbol : string; arity : int }
type binding = Function of function_info | Global of string
type t = { bindings : binding Names.t; global_symbols : string list; mutable next_label : int }
exception Error of string

let create program =
  let builtins =
    Names.singleton "print_int"
      (Function { symbol = Abi.Runtime.print_int; arity = Abi.Runtime.print_int_arity })
  in
  let bindings, reversed_globals, _ =
    List.fold_left
      (fun (bindings, globals, index) -> function
        | ADecl (_, PVar name, parameters, _) ->
          if Names.mem name bindings then raise (Error ("RISC-V: duplicate top-level name: " ^ name));
          let binding, globals =
            match parameters with
            | [] ->
              let symbol = Printf.sprintf "bd_global_%d" index in
              Global symbol, symbol :: globals
            | _ ->
              Function { symbol = Printf.sprintf "bd_fn_%d" index; arity = List.length parameters }, globals
          in
          Names.add name binding bindings, globals, index + 1
        | ADecl (_, (PUnit | PUnderscore), _ :: _, _) ->
          raise (Error "RISC-V: a function definition must bind a name")
        | ADecl (_, (PUnit | PUnderscore), [], _) | AEval _ -> bindings, globals, index)
      (builtins, [], 0) program
  in
  { bindings; global_symbols = List.rev reversed_globals; next_label = 0 }
;;

let find symbols name = Names.find_opt name symbols.bindings
let require symbols name =
  match find symbols name with
  | Some binding -> binding
  | None -> raise (Error ("RISC-V: unknown identifier: " ^ name))
;;

let fresh_label symbols =
  let label = Printf.sprintf ".Lbd_%d" symbols.next_label in
  symbols.next_label <- symbols.next_label + 1;
  label
;;

let globals symbols = symbols.global_symbols
