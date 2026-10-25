[@@@ocaml.text "/*"]

(** Copyright 2026, Tenyaeva Ekaterina *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
module SSet = Set.Make (String)
module SMap = Map.Make (String)

(* ---------- free variables ---------- *)

let fv_imm = function
  | ImmVar name -> SSet.singleton name
  | ImmInt _ | ImmBool _ | ImmUnit -> SSet.empty
;;

let fv_imms imms =
  List.fold_left (fun acc imm -> SSet.union acc (fv_imm imm)) SSet.empty imms
;;

let rec fv_cexpr = function
  | CImm imm -> fv_imm imm
  | CApp (fn, args) -> fv_imms (fn :: args)
  | CTuple (first, second, rest) -> fv_imms (first :: second :: rest)
  | CIte (cond, then_branch, else_branch) ->
    SSet.union (fv_imm cond) (SSet.union (fv_aexpr then_branch) (fv_aexpr else_branch))
  | CFun (params, body) -> SSet.diff (fv_aexpr body) (SSet.of_list params)
  | CMatchFailure -> SSet.empty

and fv_aexpr = function
  | ACExpr cexpr -> fv_cexpr cexpr
  | ALet (name, value, body) ->
    SSet.union (fv_cexpr value) (SSet.remove name (fv_aexpr body))
  | ALetTuple (names, subject, body) ->
    let bound =
      List.fold_left
        (fun acc name ->
           match name with
           | Some name -> SSet.add name acc
           | None -> acc)
        SSet.empty
        names
    in
    SSet.union (fv_imm subject) (SSet.diff (fv_aexpr body) bound)
  | ALetRec (bindings, body) ->
    let names =
      List.fold_left (fun acc (name, _) -> SSet.add name acc) SSet.empty bindings
    in
    let fv =
      List.fold_left
        (fun acc (_, value) -> SSet.union acc (fv_cexpr value))
        (fv_aexpr body)
        bindings
    in
    SSet.diff fv names
;;

(* ---------- conversion ---------- *)

let ( let* ) = Fresh.( let* )
let return = Fresh.return

type ctx =
  { locals : SSet.t
  ; recursive : imm list SMap.t
  }

let with_local ctx name =
  { locals = SSet.add name ctx.locals; recursive = SMap.remove name ctx.recursive }
;;

let with_locals ctx names = List.fold_left with_local ctx names
let imm_vars names = List.map (fun name -> ImmVar name) names
let tail expr = return (ACExpr expr)

let caps_of ctx ~params body =
  SSet.elements (SSet.inter (SSet.diff (fv_aexpr body) (SSet.of_list params)) ctx.locals)
;;

let recursive_args ctx = function
  | ImmVar name -> Option.value (SMap.find_opt name ctx.recursive) ~default:[]
  | ImmInt _ | ImmBool _ | ImmUnit -> []
;;

(* A recursive function used as a value needs its captured arguments applied.
   Introduce a temporary only where ANF requires an immediate. *)
let rec cc_imm ctx imm k =
  match imm, recursive_args ctx imm with
  | ImmVar name, (_ :: _ as captures) ->
    cc_imms ctx captures (fun captures ->
      let* temp = Fresh.name ~suffix:"__cc" name in
      let* body = k (ImmVar temp) in
      return (ALet (temp, CApp (imm, captures), body)))
  | _ -> k imm

and cc_imms ctx imms k =
  match imms with
  | [] -> k []
  | imm :: rest ->
    cc_imm ctx imm (fun imm -> cc_imms ctx rest (fun rest -> k (imm :: rest)))

and cc_cexpr ctx expr k =
  match expr with
  | CImm imm ->
    (match recursive_args ctx imm with
     | [] -> k expr
     | captures -> cc_imms ctx captures (fun captures -> k (CApp (imm, captures))))
  | CApp (fn, args) ->
    cc_imms ctx (recursive_args ctx fn @ args) (fun args -> k (CApp (fn, args)))
  | CTuple (first, second, rest) ->
    cc_imm ctx first (fun first ->
      cc_imm ctx second (fun second ->
        cc_imms ctx rest (fun rest -> k (CTuple (first, second, rest)))))
  | CIte (cond, then_branch, else_branch) ->
    cc_imm ctx cond (fun cond ->
      let* then_branch = cc_aexpr ctx then_branch in
      let* else_branch = cc_aexpr ctx else_branch in
      k (CIte (cond, then_branch, else_branch)))
  | CMatchFailure | CFun _ -> k expr

and cc_aexpr ctx = function
  | ACExpr (CFun (params, body)) -> convert_tail ctx params body
  | ACExpr cexpr -> cc_cexpr ctx cexpr tail
  | ALet (name, CFun (params, value), body) -> convert_let ctx name params value body
  | ALet (name, value, body) ->
    cc_cexpr ctx value (fun value ->
      let* body = cc_aexpr (with_local ctx name) body in
      return (ALet (name, value, body)))
  | ALetTuple (names, subject, body) ->
    cc_imm ctx subject (fun subject ->
      let ctx = with_locals ctx (List.filter_map Fun.id names) in
      let* body = cc_aexpr ctx body in
      return (ALetTuple (names, subject, body)))
  | ALetRec (bindings, body) ->
    let* ctx, bindings = convert_rec ctx bindings in
    let* body = cc_aexpr ctx body in
    return (ALetRec (bindings, body))

and convert_let ctx name params value body =
  let captures = caps_of ctx ~params value in
  let* value' = cc_aexpr (with_locals ctx (captures @ params)) value in
  let* body' = cc_aexpr (with_local ctx name) body in
  match captures with
  | [] -> return (ALet (name, CFun (params, value'), body'))
  | _ ->
    let* helper = Fresh.name ~suffix:"__cc" name in
    cc_imms ctx (imm_vars captures) (fun args ->
      return
        (ALet
           ( helper
           , CFun (captures @ params, value')
           , ALet (name, CApp (ImmVar helper, args), body') )))

and convert_tail ctx params value =
  let captures = caps_of ctx ~params value in
  let* value' = cc_aexpr (with_locals ctx (captures @ params)) value in
  match captures with
  | [] -> return (ACExpr (CFun (params, value')))
  | _ ->
    let* helper = Fresh.name ~suffix:"__cc" "f" in
    cc_imms ctx (imm_vars captures) (fun args ->
      return
        (ALet
           (helper, CFun (captures @ params, value'), ACExpr (CApp (ImmVar helper, args)))))

and convert_rec ctx bindings =
  let names = List.map fst bindings in
  let group = SSet.of_list names in
  let free =
    List.fold_left
      (fun free (_, value) -> SSet.union free (fv_cexpr value))
      SSet.empty
      bindings
  in
  (* All members receive the same captures, including values used only by
     another member. Calls within the group can then pass them directly. *)
  let captures = SSet.elements (SSet.inter ctx.locals (SSet.diff free group)) in
  let ctx = with_locals ctx names in
  let ctx =
    { ctx with
      recursive =
        List.fold_left
          (fun env name -> SMap.add name (imm_vars captures) env)
          ctx.recursive
          names
    }
  in
  let* bindings =
    Fresh.map_list
      (fun (name, value) ->
         match value with
         | CFun (params, body) ->
           let* body = cc_aexpr (with_locals ctx (captures @ params)) body in
           return (name, CFun (captures @ params, body))
         | _ -> return (name, value))
      bindings
  in
  return (ctx, bindings)
;;

let cc_item ctx = function
  | AValue (name, body) ->
    let* body = cc_aexpr ctx body in
    return (AValue (name, body))
  | ARec bindings ->
    let* _, bindings = convert_rec ctx bindings in
    return (ARec bindings)
  | AEval body ->
    let* body = cc_aexpr ctx body in
    return (AEval body)
;;

let program program =
  let ctx = { locals = SSet.empty; recursive = SMap.empty } in
  Fresh.run (Fresh.create_from_program program) (Fresh.map_list (cc_item ctx) program)
;;
