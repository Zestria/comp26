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

(* ---------- context, state and environment ---------- *)

type ctx =
  { all_names : SSet.t
  ; rebound : SSet.t
  }

(* What the conversion learns as it goes; threaded through [_ m]. *)
type state =
  { generated : SSet.t
  ; temps : SSet.t
  ; counter : int
  ; used : SSet.t
  }

type 'a m = state -> 'a * state

let return value state = value, state

let ( let* ) computation f state =
  let value, state = computation state in
  f value state
;;

let map_m f items state =
  let reversed, state =
    List.fold_left
      (fun (acc, state) item ->
         let value, state = f item state in
         value :: acc, state)
      ([], state)
      items
  in
  List.rev reversed, state
;;

let map2_m f xs ys = map_m (fun (x, y) -> f x y) (List.combine xs ys)
let get_used state = state.used, state
let set_used used state = (), { state with used }

type env =
  { subst : imm SMap.t
  ; tuples : imm list SMap.t
  }

let empty_env = { subst = SMap.empty; tuples = SMap.empty }
let lookup env name = Option.value (SMap.find_opt name env.subst) ~default:(ImmVar name)
let add_subst env name imm = { env with subst = SMap.add name imm env.subst }

let is_taken ctx state name =
  SSet.mem name ctx.all_names || SSet.mem name state.generated || SSet.mem name state.used
;;

let claim name state = name, { state with used = SSet.add name state.used }

let generate name state =
  claim name { state with generated = SSet.add name state.generated }
;;

let fresh_temp ctx state =
  let rec attempt counter =
    let name = "t" ^ Int.to_string counter in
    if is_taken ctx state name
    then attempt (counter + 1)
    else generate name { state with counter; temps = SSet.add name state.temps }
  in
  attempt (state.counter + 1)
;;

(* Renames a local binder only if its name is already taken in the item. *)
let bind_name ctx name state =
  if not (SSet.mem name state.used)
  then claim name state
  else (
    let rec attempt index =
      let candidate = Printf.sprintf "%s_%d" name index in
      if is_taken ctx state candidate
      then attempt (index + 1)
      else generate candidate state
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
let return_tail cexpr = return (tail cexpr)

(* [k] is called exactly once, so no code is duplicated.

   Where two parts are converted one after another, the order matters: it
   decides which of two equal binders gets the [_1] suffix. In a conditional
   the else branch (or the rest of the cases) goes first, and a temporary is
   drawn after the code that follows it. *)
let rec to_cexpr ctx env expr (k : cexpr -> aexpr m) : aexpr m =
  match expr with
  | EConst constant -> k (CImm (imm_of_constant constant))
  | EIdent name -> k (CImm (lookup env name))
  | EApply _ ->
    let fn, args = collect_args expr [] in
    to_imms ctx env args (fun args -> apply ctx env fn args k)
  | EFun _ | EFunction _ ->
    let* fn = lambda ctx env expr in
    k fn
  | ETuple (first, second, rest) ->
    to_imm ctx env first (fun first ->
      to_imm ctx env second (fun second ->
        to_imms ctx env rest (fun rest -> k (CTuple (first, second, rest)))))
  | EIf (cond, then_branch, else_branch) ->
    let else_branch = Option.value else_branch ~default:(EConst CUnit) in
    to_imm ctx env cond (function
      | ImmBool true -> to_cexpr ctx env then_branch k
      | ImmBool false -> to_cexpr ctx env else_branch k
      | cond ->
        let* else_branch = to_tail ctx env else_branch in
        let* then_branch = to_tail ctx env then_branch in
        k (CIte (cond, then_branch, else_branch)))
  | ELet (NonRec, first, rest, body) ->
    bind_nonrec ctx env env (first :: rest) (fun env -> to_cexpr ctx env body k)
  | ELet (Rec, first, rest, body) ->
    let* env, bindings = bind_rec ctx env ~rename:true (first :: rest) in
    let* body = to_cexpr ctx env body k in
    return (ALetRec (bindings, body))
  | EMatch (subject, first, rest) ->
    to_shape ctx env subject (fun shape -> compile_cases ctx env shape (first :: rest) k)

and to_tail ctx env expr = to_cexpr ctx env expr return_tail

and to_imm ctx env expr k =
  to_cexpr ctx env expr (function
    | CImm imm -> k imm
    | cexpr ->
      let* name = fresh_temp ctx in
      let* body = k (ImmVar name) in
      return (ALet (name, cexpr, body)))

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
  let* outer = get_used in
  let rec params env acc refutable = function
    | EFun (pat, body) ->
      (match pat with
       | PVar name ->
         let* renamed = bind_name ctx name in
         params (add_subst env name (ImmVar renamed)) (renamed :: acc) refutable body
       | PAny | PConstant CUnit ->
         let* name = fresh_temp ctx in
         params env (name :: acc) refutable body
       | PConstant _ | PTuple _ ->
         let* name = fresh_temp ctx in
         params env (name :: acc) ((name, pat) :: refutable) body)
    | EFunction (first, rest) ->
      let* name = fresh_temp ctx in
      return (List.rev (name :: acc), env, refutable, EMatch (EIdent name, first, rest))
    | body -> return (List.rev acc, env, refutable, body)
  in
  let* names, env, refutable, body = params env [] [] expr in
  let body =
    List.fold_left
      (fun body (name, pat) ->
         EMatch (EIdent name, { case_pat = pat; case_expr = body }, []))
      body
      refutable
  in
  let* body = to_tail ctx env body in
  let* () = set_used outer in
  return (CFun (names, body))

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
           let* renamed = bind_name ctx name in
           let* body = continue (add_subst into name (ImmVar renamed)) in
           return (ALet (renamed, cexpr, body)))
     | PAny | PConstant CUnit ->
       to_cexpr ctx env vb_expr (function
         | CImm _ -> continue into
         | cexpr ->
           let* body = continue into in
           let* name = fresh_temp ctx in
           return (ALet (name, cexpr, body)))
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
  let* renamed =
    map_m (fun name -> if rename then bind_name ctx name else claim name) names
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
    | EFun _ | EFunction _ ->
      let* fn = lambda ctx env vb_expr in
      return (renamed, fn)
    | _ -> raise (Unsupported "the right-hand side of let rec must be a function")
  in
  let* bindings = map2_m compile renamed bindings in
  return (env, bindings)

and bind_shape ctx env shape pat k =
  match_pattern ctx env shape pat (fun outcome _ matched ->
    match outcome with
    | Never -> return_tail CMatchFailure
    | Always -> k matched
    | Cond cond ->
      let* body = k matched in
      return_tail (CIte (cond, body, tail CMatchFailure)))

and compile_cases ctx env shape cases k =
  match cases with
  | [] -> k CMatchFailure
  | { case_pat; case_expr } :: rest ->
    match_pattern ctx env shape case_pat (fun outcome env matched ->
      match outcome with
      | Never -> compile_cases ctx env shape rest k
      | Always -> to_cexpr ctx matched case_expr k
      | Cond cond ->
        let* else_branch = compile_cases ctx env shape rest return_tail in
        let* then_branch = to_tail ctx matched case_expr in
        k (CIte (cond, then_branch, else_branch)))

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
       let* names =
         map_m
           (function
             | PVar var -> bind_name ctx var
             | _ -> fresh_temp ctx)
           pats
       in
       let imms = List.map (fun name -> ImmVar name) names in
       let env = { env with tuples = SMap.add name imms env.tuples } in
       let* body = k env imms in
       return (ALetTuple (List.map Option.some names, subject, body)))
  | _ -> raise (Unsupported "a tuple pattern cannot match a constant")

and emit_tests ctx tests k =
  let test (subject, expected) k =
    match expected with
    | ImmBool true -> k subject
    | _ ->
      let* name = fresh_temp ctx in
      let* body = k (ImmVar name) in
      return (ALet (name, CApp (ImmVar "=", [ subject; expected ]), body))
  in
  let rec conjunction acc = function
    | [] -> k acc
    | check :: rest ->
      test check (fun cond ->
        match acc with
        | None -> conjunction (Some cond) rest
        | Some acc ->
          let* name = fresh_temp ctx in
          let* body = conjunction (Some (ImmVar name)) rest in
          return (ALet (name, CApp (ImmVar "&&", [ acc; cond ]), body)))
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
        let* name = fresh_temp ctx in
        let* body = k (ImmVar name) in
        return (ALet (name, CTuple (first, second, rest), body))
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
  let* () = set_used (free_vars_item item) in
  match item with
  | SEval expr ->
    let* body = to_tail ctx empty_env expr in
    return [ AEval body ]
  | SValue (NonRec, { vb_pat = PVar name; vb_expr }, []) ->
    let* body = to_tail ctx empty_env vb_expr in
    return [ AValue (name, body) ]
  | SValue (NonRec, { vb_pat = PAny | PConstant CUnit; vb_expr }, []) ->
    let* body = to_tail ctx empty_env vb_expr in
    return [ AEval body ]
  | SValue (NonRec, first, rest) ->
    let bindings = first :: rest in
    let* temps =
      map_m
        (fun { vb_expr; _ } ->
           let* body = to_tail ctx empty_env vb_expr in
           let* temp = fresh_temp ctx in
           return (temp, body))
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
      | PVar name, _ -> return [ AValue (name, tail (CImm (ImmVar temp))) ]
      | _, [] ->
        let* body = select (EConst CUnit) in
        return [ AEval body ]
      | _, vars ->
        map_m
          (fun var ->
             let* body = select (EIdent var) in
             return (AValue (var, body)))
          vars
    in
    let* projected = map2_m project temps bindings in
    return
      (List.map (fun (temp, body) -> AValue (temp, body)) temps @ List.concat projected)
  | SValue (Rec, first, rest) ->
    let* _, bindings = bind_rec ctx empty_env ~rename:false (first :: rest) in
    return [ ARec bindings ]
;;

let all_names structure =
  union_map (fun item -> SSet.union (free_vars_item item) (binders_item item)) structure
;;

(* ---------- renumbering of temporaries ---------- *)

(* Temporaries get the names t1, t2, ... in the order their binders appear in
   the program. The first pass picks the new names, the second renames. *)
let renumber ctx state program =
  let taken = SSet.union ctx.all_names (SSet.diff state.generated state.temps) in
  let rec next counter =
    let name = "t" ^ Int.to_string counter in
    if SSet.mem name taken then next (counter + 1) else name, counter
  in
  let bind (names, counter) name =
    if SSet.mem name state.temps
    then (
      let renamed, counter = next (counter + 1) in
      SMap.add name renamed names, counter)
    else names, counter
  in
  let rec bind_cexpr acc = function
    | CImm _ | CApp _ | CTuple _ | CMatchFailure -> acc
    | CIte (_, then_branch, else_branch) ->
      bind_aexpr (bind_aexpr acc then_branch) else_branch
    | CFun (params, body) -> bind_aexpr (List.fold_left bind acc params) body
  and bind_aexpr acc = function
    | ALet (name, value, body) -> bind_aexpr (bind_cexpr (bind acc name) value) body
    | ALetTuple (slots, _, body) ->
      let acc =
        List.fold_left
          (fun acc slot -> Option.fold ~none:acc ~some:(bind acc) slot)
          acc
          slots
      in
      bind_aexpr acc body
    | ALetRec (bindings, body) -> bind_aexpr (bind_rec_bindings acc bindings) body
    | ACExpr value -> bind_cexpr acc value
  and bind_rec_bindings acc bindings =
    let acc = List.fold_left (fun acc (name, _) -> bind acc name) acc bindings in
    List.fold_left (fun acc (_, value) -> bind_cexpr acc value) acc bindings
  in
  let bind_item acc = function
    | AValue (name, body) -> bind_aexpr (bind acc name) body
    | ARec bindings -> bind_rec_bindings acc bindings
    | AEval body -> bind_aexpr acc body
  in
  let names, _ = List.fold_left bind_item (SMap.empty, 0) program in
  let rename name = Option.value (SMap.find_opt name names) ~default:name in
  let imm = function
    | ImmVar name -> ImmVar (rename name)
    | other -> other
  in
  let rec cexpr = function
    | CImm value -> CImm (imm value)
    | CApp (fn, args) -> CApp (imm fn, List.map imm args)
    | CTuple (first, second, rest) -> CTuple (imm first, imm second, List.map imm rest)
    | CIte (cond, then_branch, else_branch) ->
      CIte (imm cond, aexpr then_branch, aexpr else_branch)
    | CFun (params, body) -> CFun (List.map rename params, aexpr body)
    | CMatchFailure -> CMatchFailure
  and aexpr = function
    | ALet (name, value, body) -> ALet (rename name, cexpr value, aexpr body)
    | ALetTuple (slots, subject, body) ->
      ALetTuple (List.map (Option.map rename) slots, imm subject, aexpr body)
    | ALetRec (bindings, body) -> ALetRec (rec_bindings bindings, aexpr body)
    | ACExpr value -> ACExpr (cexpr value)
  and rec_bindings bindings =
    List.map (fun (name, value) -> rename name, cexpr value) bindings
  in
  let item = function
    | AValue (name, body) -> AValue (rename name, aexpr body)
    | ARec bindings -> ARec (rec_bindings bindings)
    | AEval body -> AEval (aexpr body)
  in
  List.map item program
;;

let convert structure =
  let ctx =
    { all_names = all_names structure; rebound = union_map binders_item structure }
  in
  let state =
    { generated = SSet.empty; temps = SSet.empty; counter = 0; used = SSet.empty }
  in
  match map_m (convert_item ctx) structure state with
  | exception Unsupported message -> Error message
  | converted ->
    let items, state = converted in
    let program = Anf_simplify.program ~is_prim:(is_prim ctx) (List.concat items) in
    Ok (renumber ctx state program)
;;
