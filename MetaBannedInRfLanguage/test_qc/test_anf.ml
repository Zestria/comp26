[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

let lookup_value find env name =
  match find name env with
  | Some v -> Ok v
  | None -> Error (Printf.sprintf "name %S is not bound after evaluation" name)
;;

let same_bound_values names ast_env anf_env =
  let ( let* ) = Result.bind in
  let rec go = function
    | [] -> Ok true
    | name :: rest ->
      let* ast_value = lookup_value Ast_interp.SMap.find_opt ast_env name in
      let* anf_value = lookup_value Anf_interp.SMap.find_opt anf_env name in
      if Value.equal ast_value anf_value then go rest else Ok false
  in
  go names
;;

let anf_preserves_semantics structure =
  match Anf_conv.convert structure with
  | Error _ -> true
  | Ok program ->
    (match Ast_interp.eval_structure structure, Anf_interp.eval_program program with
     | Ok ast_env, Ok anf_env ->
       (match same_bound_values (structure_bound_names structure) ast_env anf_env with
        | Ok result -> result
        | Error _ -> false)
     | Error _, Error _ -> true
     | Ok _, Error _ | Error _, Ok _ -> false)
;;

let test_anf_preserves_semantics =
  QCheck.Test.make
    ~name:"ANF conversion preserves semantics: eval(ast) = eval(anf)"
    ~count:300
    Typed_gen.arb_structure
    anf_preserves_semantics
;;

let alpha_preserves_semantics expr =
  let renamed = Alpha.rename_expression expr in
  match
    Ast_interp.eval_expression Ast_interp.SMap.empty expr,
    Ast_interp.eval_expression Ast_interp.SMap.empty renamed
  with
  | Ok v1, Ok v2 -> Value.equal v1 v2
  | Error _, Error _ -> true
  | Ok _, Error _ | Error _, Ok _ -> false
;;

let test_alpha_preserves_semantics =
  QCheck.Test.make
    ~name:"alpha-renaming preserves semantics: eval(e) = eval(rename(e))"
    ~count:300
    Typed_gen.arb_expression
    alpha_preserves_semantics
;;

let pipeline_preserves_semantics structure =
  match Anf_conv.convert structure with
  | Error _ -> true
  | Ok program ->
    let program = Closure_conversion.program program in
    let program = Lambda_lifting.program program in
    (match Ast_interp.eval_structure structure, Anf_interp.eval_program program with
     | Ok ast_env, Ok anf_env ->
       (match same_bound_values (structure_bound_names structure) ast_env anf_env with
        | Ok result -> result
        | Error _ -> false)
     | Error _, Error _ -> true
     | Ok _, Error _ | Error _, Ok _ -> false)
;;

let test_pipeline_preserves_semantics =
  QCheck.Test.make
    ~name:"full pipeline preserves semantics: eval(ast) = eval(lambda_lifting(closure_conversion(anf)))"
    ~count:300
    Typed_gen.arb_structure
    pipeline_preserves_semantics
;;

let () =
  let tests =
    [ test_anf_preserves_semantics; test_alpha_preserves_semantics; test_pipeline_preserves_semantics ]
  in
  exit (QCheck_runner.run_tests ~verbose:true tests)
;;
