[@@@ocaml.text "/*"]

(** Copyright 2026, Lemekhov Daniil *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Ast
open Anf
module SSet = Set.Make (String)
module SMap = Map.Make (String)

exception Unsupported of string

(* ---------- names of the source program ---------- *)

let rec pattern_vars acc = function
  | PAny | PConstant _ -> acc
  | PVar name -> SSet.add name acc
  | PTuple (first, second, rest) ->
    List.fold_left pattern_vars acc (first :: second :: rest)
;;

let bound_by_bindings bindings =
  List.fold_left (fun acc { vb_pat; _ } -> pattern_vars acc vb_pat) SSet.empty bindings
;;

let union_map f items =
  List.fold_left (fun acc item -> SSet.union acc (f item)) SSet.empty items
;;

let rec free_vars = function
  | EConst _ -> SSet.empty
  | EIdent name -> SSet.singleton name
  | EFun (pat, body) -> SSet.diff (free_vars body) (pattern_vars SSet.empty pat)
  | EApply (fn, arg) -> SSet.union (free_vars fn) (free_vars arg)
  | ETuple (first, second, rest) -> union_map free_vars (first :: second :: rest)
  | EIf (cond, then_branch, else_branch) ->
    union_map free_vars (cond :: then_branch :: Option.to_list else_branch)
  | ELet (NonRec, first, rest, body) ->
    let bindings = first :: rest in
    SSet.union
      (union_map (fun { vb_expr; _ } -> free_vars vb_expr) bindings)
      (SSet.diff (free_vars body) (bound_by_bindings bindings))
  | ELet (Rec, first, rest, body) ->
    let bindings = first :: rest in
    SSet.diff
      (SSet.union
         (free_vars body)
         (union_map (fun { vb_expr; _ } -> free_vars vb_expr) bindings))
      (bound_by_bindings bindings)
  | EFunction (first, rest) -> union_map free_vars_case (first :: rest)
  | EMatch (subject, first, rest) ->
    SSet.union (free_vars subject) (union_map free_vars_case (first :: rest))

and free_vars_case { case_pat; case_expr } =
  SSet.diff (free_vars case_expr) (pattern_vars SSet.empty case_pat)
;;

let free_vars_item = function
  | SEval expr -> free_vars expr
  | SValue (NonRec, first, rest) ->
    union_map (fun { vb_expr; _ } -> free_vars vb_expr) (first :: rest)
  | SValue (Rec, first, rest) ->
    let bindings = first :: rest in
    SSet.diff
      (union_map (fun { vb_expr; _ } -> free_vars vb_expr) bindings)
      (bound_by_bindings bindings)
;;

let rec binders = function
  | EConst _ | EIdent _ -> SSet.empty
  | EFun (pat, body) -> pattern_vars (binders body) pat
  | EApply (fn, arg) -> SSet.union (binders fn) (binders arg)
  | ETuple (first, second, rest) -> union_map binders (first :: second :: rest)
  | EIf (cond, then_branch, else_branch) ->
    union_map binders (cond :: then_branch :: Option.to_list else_branch)
  | ELet (_, first, rest, body) ->
    let bindings = first :: rest in
    SSet.union
      (binders body)
      (union_map
         (fun { vb_pat; vb_expr } -> pattern_vars (binders vb_expr) vb_pat)
         bindings)
  | EFunction (first, rest) -> union_map binders_case (first :: rest)
  | EMatch (subject, first, rest) ->
    SSet.union (binders subject) (union_map binders_case (first :: rest))

and binders_case { case_pat; case_expr } = pattern_vars (binders case_expr) case_pat

let binders_item = function
  | SEval expr -> binders expr
  | SValue (_, first, rest) ->
    union_map
      (fun { vb_pat; vb_expr } -> pattern_vars (binders vb_expr) vb_pat)
      (first :: rest)
;;

(* ---------- context and environment ---------- *)

type ctx =
  { all_names : SSet.t
  ; rebound : SSet.t
  ; mutable generated : SSet.t
  ; mutable counter : int
  ; mutable used : SSet.t
  }

type env =
  { subst : imm SMap.t
  ; tuples : imm list SMap.t
  }

let empty_env = { subst = SMap.empty; tuples = SMap.empty }
let lookup env name = Option.value (SMap.find_opt name env.subst) ~default:(ImmVar name)
let add_subst env name imm = { env with subst = SMap.add name imm env.subst }

let is_taken ctx name =
  SSet.mem name ctx.all_names || SSet.mem name ctx.generated || SSet.mem name ctx.used
;;

let claim ctx name =
  ctx.used <- SSet.add name ctx.used;
  name
;;

let generate ctx name =
  ctx.generated <- SSet.add name ctx.generated;
  claim ctx name
;;

let rec fresh_temp ctx =
  ctx.counter <- ctx.counter + 1;
  let name = "t" ^ Int.to_string ctx.counter in
  if is_taken ctx name then fresh_temp ctx else generate ctx name
;;

(* Renames a local binder only if its name is already taken in the item. *)
let bind_name ctx name =
  if not (SSet.mem name ctx.used)
  then claim ctx name
  else (
    let rec attempt index =
      let candidate = Printf.sprintf "%s_%d" name index in
      if is_taken ctx candidate then attempt (index + 1) else generate ctx candidate
    in
    attempt 1)
;;

(* ---------- primitives ---------- *)

let is_prim ctx name = not (SSet.mem name ctx.rebound)

let fold_app ctx fn args =
  match fn, args with
  | ImmVar op, _ when not (is_prim ctx op) -> CApp (fn, args)
  | ImmVar "+", [ ImmInt a; ImmInt b ] -> CImm (ImmInt (a + b))
  | ImmVar "-", [ ImmInt a; ImmInt b ] -> CImm (ImmInt (a - b))
  | ImmVar "*", [ ImmInt a; ImmInt b ] -> CImm (ImmInt (a * b))
  | ImmVar "/", [ ImmInt a; ImmInt b ] when b <> 0 -> CImm (ImmInt (a / b))
  | ImmVar "-", [ ImmInt a ] -> CImm (ImmInt (-a))
  | ImmVar "+", [ (ImmInt _ as a) ] -> CImm a
  | ImmVar "<", [ ImmInt a; ImmInt b ] -> CImm (ImmBool (a < b))
  | ImmVar "<=", [ ImmInt a; ImmInt b ] -> CImm (ImmBool (a <= b))
  | ImmVar ">", [ ImmInt a; ImmInt b ] -> CImm (ImmBool (a > b))
  | ImmVar ">=", [ ImmInt a; ImmInt b ] -> CImm (ImmBool (a >= b))
  | ImmVar "=", [ ImmInt a; ImmInt b ] -> CImm (ImmBool (a = b))
  | ImmVar "<>", [ ImmInt a; ImmInt b ] -> CImm (ImmBool (a <> b))
  | ImmVar "=", [ ImmBool a; ImmBool b ] -> CImm (ImmBool (Bool.equal a b))
  | ImmVar "<>", [ ImmBool a; ImmBool b ] -> CImm (ImmBool (not (Bool.equal a b)))
  | ImmVar "not", [ ImmBool a ] -> CImm (ImmBool (not a))
  | ImmVar "&&", [ ImmBool true; other ] | ImmVar "&&", [ other; ImmBool true ] ->
    CImm other
  | ImmVar "&&", [ ImmBool false; _ ] | ImmVar "&&", [ _; ImmBool false ] ->
    CImm (ImmBool false)
  | ImmVar "||", [ ImmBool false; other ] | ImmVar "||", [ other; ImmBool false ] ->
    CImm other
  | ImmVar "||", [ ImmBool true; _ ] | ImmVar "||", [ _; ImmBool true ] ->
    CImm (ImmBool true)
  | _ -> CApp (fn, args)
;;

(* ---------- conversion ---------- *)

type shape =
  | Imm of imm
  | Tup of shape list

type outcome =
  | Never
  | Always
  | Cond of imm

let imm_of_constant = function
  | CInt n -> ImmInt n
  | CBool b -> ImmBool b
  | CUnit -> ImmUnit
;;

let rec collect_args expr args =
  match expr with
  | EApply (fn, arg) -> collect_args fn (arg :: args)
  | fn -> fn, args
;;

let tail cexpr = ACExpr cexpr

(* [k] is called exactly once, so no code is duplicated. *)
let rec to_cexpr ctx env expr k =
  match expr with
  | EConst constant -> k (CImm (imm_of_constant constant))
  | EIdent name -> k (CImm (lookup env name))
  | EApply _ ->
    let fn, args = collect_args expr [] in
    to_imms ctx env args (fun args -> apply ctx env fn args k)
  | EFun _ | EFunction _ -> k (lambda ctx env expr)
  | ETuple (first, second, rest) ->
    to_imm ctx env first (fun first ->
      to_imm ctx env second (fun second ->
        to_imms ctx env rest (fun rest -> k (CTuple (first, second, rest)))))
  | EIf (cond, then_branch, else_branch) ->
    let else_branch = Option.value else_branch ~default:(EConst CUnit) in
    to_imm ctx env cond (function
      | ImmBool true -> to_cexpr ctx env then_branch k
      | ImmBool false -> to_cexpr ctx env else_branch k
      | cond -> k (CIte (cond, to_tail ctx env then_branch, to_tail ctx env else_branch)))
  | ELet (NonRec, first, rest, body) ->
    bind_nonrec ctx env env (first :: rest) (fun env -> to_cexpr ctx env body k)
  | ELet (Rec, first, rest, body) ->
    let env, bindings = bind_rec ctx env ~rename:true (first :: rest) in
    ALetRec (bindings, to_cexpr ctx env body k)
  | EMatch (subject, first, rest) ->
    to_shape ctx env subject (fun shape -> compile_cases ctx env shape (first :: rest) k)

and to_tail ctx env expr = to_cexpr ctx env expr tail

and to_imm ctx env expr k =
  to_cexpr ctx env expr (function
    | CImm imm -> k imm
    | cexpr ->
      let name = fresh_temp ctx in
      ALet (name, cexpr, k (ImmVar name)))

and to_imms ctx env exprs k =
  match exprs with
  | [] -> k []
  | expr :: rest ->
    to_imm ctx env expr (fun imm -> to_imms ctx env rest (fun imms -> k (imm :: imms)))

and to_shape ctx env expr k =
  match expr with
  | ETuple (first, second, rest) ->
    to_shapes ctx env (first :: second :: rest) (fun shapes -> k (Tup shapes))
  | _ -> to_imm ctx env expr (fun imm -> k (Imm imm))

and to_shapes ctx env exprs k =
  match exprs with
  | [] -> k []
  | expr :: rest ->
    to_shape ctx env expr (fun shape ->
      to_shapes ctx env rest (fun shapes -> k (shape :: shapes)))

and apply ctx env fn args k =
  match fn, args with
  | _, [] -> to_cexpr ctx env fn k
  | EFun (pat, body), arg :: rest ->
    bind_shape ctx env (Imm arg) pat (fun env -> apply ctx env body rest k)
  | _ -> to_imm ctx env fn (fun fn -> k (fold_app ctx fn args))

and lambda ctx env expr =
  let rec params env acc refutable = function
    | EFun (pat, body) ->
      (match pat with
       | PVar name ->
         let renamed = bind_name ctx name in
         params (add_subst env name (ImmVar renamed)) (renamed :: acc) refutable body
       | PAny | PConstant CUnit -> params env (fresh_temp ctx :: acc) refutable body
       | PConstant _ | PTuple _ ->
         let name = fresh_temp ctx in
         params env (name :: acc) ((name, pat) :: refutable) body)
    | EFunction (first, rest) ->
      let name = fresh_temp ctx in
      List.rev (name :: acc), env, refutable, EMatch (EIdent name, first, rest)
    | body -> List.rev acc, env, refutable, body
  in
  let names, env, refutable, body = params env [] [] expr in
  let body =
    List.fold_left
      (fun body (name, pat) ->
         EMatch (EIdent name, { case_pat = pat; case_expr = body }, []))
      body
      refutable
  in
  CFun (names, to_tail ctx env body)

and bind_nonrec ctx env into bindings k =
  match bindings with
  | [] -> k into
  | { vb_pat; vb_expr } :: rest ->
    let continue into = bind_nonrec ctx env into rest k in
    (match vb_pat with
     | PVar name ->
       to_cexpr ctx env vb_expr (function
         | CImm imm -> continue (add_subst into name imm)
         | cexpr ->
           let renamed = bind_name ctx name in
           ALet (renamed, cexpr, continue (add_subst into name (ImmVar renamed))))
     | PAny | PConstant CUnit ->
       to_cexpr ctx env vb_expr (function
         | CImm _ -> continue into
         | cexpr -> ALet (fresh_temp ctx, cexpr, continue into))
     | PConstant _ | PTuple _ ->
       to_shape ctx env vb_expr (fun shape -> bind_shape ctx into shape vb_pat continue))

and bind_rec ctx env ~rename bindings =
  let names =
    List.map
      (fun { vb_pat; _ } ->
         match vb_pat with
         | PVar name -> name
         | _ -> raise (Unsupported "only variables can be bound by let rec"))
      bindings
  in
  let renamed =
    List.map (fun name -> if rename then bind_name ctx name else claim ctx name) names
  in
  let env =
    List.fold_left2
      (fun env name renamed -> add_subst env name (ImmVar renamed))
      env
      names
      renamed
  in
  let compile renamed { vb_expr; _ } =
    match vb_expr with
    | EFun _ | EFunction _ -> renamed, lambda ctx env vb_expr
    | _ -> raise (Unsupported "the right-hand side of let rec must be a function")
  in
  env, List.map2 compile renamed bindings

and bind_shape ctx env shape pat k =
  match_pattern ctx env shape pat (fun outcome _ matched ->
    match outcome with
    | Never -> tail CMatchFailure
    | Always -> k matched
    | Cond cond -> tail (CIte (cond, k matched, tail CMatchFailure)))

and compile_cases ctx env shape cases k =
  match cases with
  | [] -> k CMatchFailure
  | { case_pat; case_expr } :: rest ->
    match_pattern ctx env shape case_pat (fun outcome env matched ->
      match outcome with
      | Never -> compile_cases ctx env shape rest k
      | Always -> to_cexpr ctx matched case_expr k
      | Cond cond ->
        k
          (CIte
             (cond, to_tail ctx matched case_expr, compile_cases ctx env shape rest tail)))

and match_pattern ctx env shape pat k =
  destruct ctx env shape pat (Some []) [] (fun env tests vars ->
    match tests with
    | None -> k Never env env
    | Some tests ->
      emit_tests ctx (List.rev tests) (fun cond ->
        bind_vars ctx env (List.rev vars) (fun matched ->
          match cond with
          | None -> k Always env matched
          | Some cond -> k (Cond cond) env matched)))

and destruct ctx env shape pat tests vars k =
  match pat, shape with
  | PAny, _ | PConstant CUnit, _ -> k env tests vars
  | PVar name, _ -> k env tests ((name, shape) :: vars)
  | PConstant constant, Imm subject ->
    let expected = imm_of_constant constant in
    let tests =
      match subject, tests with
      | _, None -> None
      | ImmVar _, Some tests -> Some ((subject, expected) :: tests)
      | _, Some tests -> if subject = expected then Some tests else None
    in
    k env tests vars
  | PConstant _, Tup _ -> raise (Unsupported "a constant pattern cannot match a tuple")
  | PTuple (first, second, rest), Tup shapes ->
    destruct_list ctx env shapes (first :: second :: rest) tests vars k
  | PTuple (first, second, rest), Imm subject ->
    let pats = first :: second :: rest in
    components ctx env subject pats (fun env imms ->
      destruct_list ctx env (List.map (fun imm -> Imm imm) imms) pats tests vars k)

and destruct_list ctx env shapes pats tests vars k =
  match shapes, pats with
  | [], [] -> k env tests vars
  | shape :: shapes, pat :: pats ->
    destruct ctx env shape pat tests vars (fun env tests vars ->
      destruct_list ctx env shapes pats tests vars k)
  | _ -> raise (Unsupported "tuple pattern of a wrong size")

and components ctx env subject pats k =
  match subject with
  | ImmVar name ->
    (match SMap.find_opt name env.tuples with
     | Some imms when List.compare_lengths imms pats = 0 -> k env imms
     | _ ->
       let names =
         List.map
           (function
             | PVar var -> bind_name ctx var
             | _ -> fresh_temp ctx)
           pats
       in
       let imms = List.map (fun name -> ImmVar name) names in
       let env = { env with tuples = SMap.add name imms env.tuples } in
       ALetTuple (List.map Option.some names, subject, k env imms))
  | _ -> raise (Unsupported "a tuple pattern cannot match a constant")

and emit_tests ctx tests k =
  let test (subject, expected) k =
    match expected with
    | ImmBool true -> k subject
    | _ ->
      let name = fresh_temp ctx in
      ALet (name, CApp (ImmVar "=", [ subject; expected ]), k (ImmVar name))
  in
  let rec conjunction acc = function
    | [] -> k acc
    | check :: rest ->
      test check (fun cond ->
        match acc with
        | None -> conjunction (Some cond) rest
        | Some acc ->
          let name = fresh_temp ctx in
          ALet
            ( name
            , CApp (ImmVar "&&", [ acc; cond ])
            , conjunction (Some (ImmVar name)) rest ))
  in
  conjunction None tests

and bind_vars ctx env vars k =
  match vars with
  | [] -> k env
  | (name, shape) :: rest ->
    materialize ctx shape (fun imm -> bind_vars ctx (add_subst env name imm) rest k)

and materialize ctx shape k =
  match shape with
  | Imm imm -> k imm
  | Tup shapes ->
    materialize_list ctx shapes (function
      | first :: second :: rest ->
        let name = fresh_temp ctx in
        ALet (name, CTuple (first, second, rest), k (ImmVar name))
      | _ -> raise (Unsupported "a tuple must have at least two components"))

and materialize_list ctx shapes k =
  match shapes with
  | [] -> k []
  | shape :: rest ->
    materialize ctx shape (fun imm ->
      materialize_list ctx rest (fun imms -> k (imm :: imms)))
;;

(* ---------- top level ---------- *)

let convert_item ctx item =
  ctx.used <- free_vars_item item;
  match item with
  | SEval expr -> [ AEval (to_tail ctx empty_env expr) ]
  | SValue (NonRec, { vb_pat = PVar name; vb_expr }, []) ->
    [ AValue (name, to_tail ctx empty_env vb_expr) ]
  | SValue (NonRec, { vb_pat = PAny | PConstant CUnit; vb_expr }, []) ->
    [ AEval (to_tail ctx empty_env vb_expr) ]
  | SValue (NonRec, first, rest) ->
    let bindings = first :: rest in
    let temps =
      List.map
        (fun { vb_expr; _ } -> fresh_temp ctx, to_tail ctx empty_env vb_expr)
        bindings
    in
    let project (temp, _) { vb_pat; _ } =
      let select result =
        to_tail
          ctx
          empty_env
          (EMatch (EIdent temp, { case_pat = vb_pat; case_expr = result }, []))
      in
      match vb_pat, SSet.elements (pattern_vars SSet.empty vb_pat) with
      | PVar name, _ -> [ AValue (name, tail (CImm (ImmVar temp))) ]
      | _, [] -> [ AEval (select (EConst CUnit)) ]
      | _, vars -> List.map (fun var -> AValue (var, select (EIdent var))) vars
    in
    List.map (fun (temp, body) -> AValue (temp, body)) temps
    @ List.concat (List.map2 project temps bindings)
  | SValue (Rec, first, rest) ->
    let _, bindings = bind_rec ctx empty_env ~rename:false (first :: rest) in
    [ ARec bindings ]
;;

let all_names structure =
  union_map (fun item -> SSet.union (free_vars_item item) (binders_item item)) structure
;;

let convert structure =
  let ctx =
    { all_names = all_names structure
    ; rebound = union_map binders_item structure
    ; generated = SSet.empty
    ; counter = 0
    ; used = SSet.empty
    }
  in
  match List.concat_map (convert_item ctx) structure with
  | program -> Ok program
  | exception Unsupported message -> Error message
;;
