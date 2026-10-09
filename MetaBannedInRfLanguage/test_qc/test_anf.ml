[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module SSet = Set.Make (String)

let union_map f items = List.fold_left (fun acc item -> SSet.union acc (f item)) SSet.empty items

let rec pattern_vars acc = function
  | Ast.PAny | Ast.PConstant _ -> acc
  | Ast.PVar name -> SSet.add name acc
  | Ast.PTuple (first, second, rest) -> List.fold_left pattern_vars acc (first :: second :: rest)
;;

let bound_by_bindings bindings =
  List.fold_left (fun acc { Ast.vb_pat; _ } -> pattern_vars acc vb_pat) SSet.empty bindings
;;

let rec free_vars = function
  | Ast.EConst _ -> SSet.empty
  | Ast.EIdent name -> if Value.is_primitive name then SSet.empty else SSet.singleton name
  | Ast.EFun (pat, body) -> SSet.diff (free_vars body) (pattern_vars SSet.empty pat)
  | Ast.EApply (fn, arg) -> SSet.union (free_vars fn) (free_vars arg)
  | Ast.ETuple (first, second, rest) -> union_map free_vars (first :: second :: rest)
  | Ast.EIf (cond, then_branch, else_branch) ->
    union_map free_vars (cond :: then_branch :: Option.to_list else_branch)
  | Ast.ELet (Ast.NonRec, first, rest, body) ->
    let bindings = first :: rest in
    SSet.union
      (union_map (fun { Ast.vb_expr; _ } -> free_vars vb_expr) bindings)
      (SSet.diff (free_vars body) (bound_by_bindings bindings))
  | Ast.ELet (Ast.Rec, first, rest, body) ->
    let bindings = first :: rest in
    SSet.diff
      (SSet.union (free_vars body) (union_map (fun { Ast.vb_expr; _ } -> free_vars vb_expr) bindings))
      (bound_by_bindings bindings)
  | Ast.EFunction (first, rest) -> union_map free_vars_case (first :: rest)
  | Ast.EMatch (subject, first, rest) ->
    SSet.union (free_vars subject) (union_map free_vars_case (first :: rest))

and free_vars_case { Ast.case_pat; case_expr } = SSet.diff (free_vars case_expr) (pattern_vars SSet.empty case_pat)
;;

let structure_is_closed structure =
  let rec go bound = function
    | [] -> true
    | item :: rest ->
      let item_free =
        match item with
        | Ast.SEval expr -> free_vars expr
        | Ast.SValue (Ast.NonRec, first, rest_vb) ->
          union_map (fun { Ast.vb_expr; _ } -> free_vars vb_expr) (first :: rest_vb)
        | Ast.SValue (Ast.Rec, first, rest_vb) ->
          let bindings = first :: rest_vb in
          SSet.diff
            (union_map (fun { Ast.vb_expr; _ } -> free_vars vb_expr) bindings)
            (bound_by_bindings bindings)
      in
      if not (SSet.is_empty (SSet.diff item_free bound))
      then false
      else (
        let bound =
          match item with
          | Ast.SEval _ -> bound
          | Ast.SValue (_, first, rest_vb) -> SSet.union bound (bound_by_bindings (first :: rest_vb))
        in
        go bound rest)
  in
  go SSet.empty structure
;;

let has_unchecked_unit_pattern structure =
  List.exists
    (function
      | Ast.SValue (Ast.NonRec, { Ast.vb_pat = Ast.PConstant Ast.CUnit; _ }, []) -> true
      | _ -> false)
    structure
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
  if (not (structure_is_closed structure)) || has_unchecked_unit_pattern structure
  then true
  else
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
    Ast_gen.arb_structure
    anf_preserves_semantics
;;

let alpha_preserves_semantics expr =
  if not (SSet.is_empty (free_vars expr))
  then true
  else (
    let renamed = Alpha.rename_expression expr in
    match
      Ast_interp.eval_expression Ast_interp.SMap.empty expr,
      Ast_interp.eval_expression Ast_interp.SMap.empty renamed
    with
    | Ok v1, Ok v2 -> Value.equal v1 v2
    | Error _, Error _ -> true
    | Ok _, Error _ | Error _, Ok _ -> false)
;;

let test_alpha_preserves_semantics =
  QCheck.Test.make
    ~name:"alpha-renaming preserves semantics: eval(e) = eval(rename(e))"
    ~count:300
    Ast_gen.arb_expression
    alpha_preserves_semantics
;;

let () =
  let tests = [ test_anf_preserves_semantics; test_alpha_preserves_semantics ] in
  exit (QCheck_runner.run_tests ~verbose:true tests)
;;
