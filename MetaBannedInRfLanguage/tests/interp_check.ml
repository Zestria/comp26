[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

let fail message =
  prerr_endline ("interp_check: " ^ message);
  exit 1
;;

let read_file path =
  try In_channel.with_open_bin path In_channel.input_all with
  | Sys_error message -> fail message
;;

let rec pattern_names acc = function
  | Ast.PAny | Ast.PConstant _ -> acc
  | Ast.PVar name -> name :: acc
  | Ast.PTuple (first, second, rest) -> List.fold_left pattern_names acc (first :: second :: rest)
;;

let structure_bound_names structure =
  List.concat_map
    (function
      | Ast.SEval _ -> []
      | Ast.SValue (_, first, rest) ->
        List.concat_map (fun { Ast.vb_pat; _ } -> pattern_names [] vb_pat) (first :: rest))
    structure
;;

let report structure ast_env anf_env =
  List.iter
    (fun name ->
       match Ast_interp.SMap.find_opt name ast_env, Anf_interp.SMap.find_opt name anf_env with
       | Some (Value.VClosure _), Some (Value.VClosure _) -> ()
       | Some ast_value, Some anf_value ->
         if Value.equal ast_value anf_value
         then Printf.printf "%s = %s\n" name (Value.to_string ast_value)
         else
           Printf.printf
             "%s: MISMATCH ast=%s anf=%s\n"
             name
             (Value.to_string ast_value)
             (Value.to_string anf_value)
       | None, _ | _, None -> Printf.printf "%s: not bound after evaluation\n" name)
    (structure_bound_names structure)
;;

let () =
  let path =
    match Sys.argv with
    | [| _; path |] -> path
    | _ -> fail "usage: interp_check.exe <input.ml>"
  in
  let source = read_file path in
  match Rd_parser.parse_structure source with
  | Error message -> fail ("parse error: " ^ message)
  | Ok structure ->
    (match Anf_conv.convert structure with
     | Error message -> fail ("Anf_conv error: " ^ message)
     | Ok program ->
       let program = Lambda_lifting.program (Closure_conversion.program program) in
       (match Ast_interp.eval_structure structure, Anf_interp.eval_program program with
        | Error message, _ -> fail ("AST evaluation error: " ^ message)
        | _, Error message -> fail ("ANF evaluation error: " ^ message)
        | Ok ast_env, Ok anf_env -> report structure ast_env anf_env))
;;
