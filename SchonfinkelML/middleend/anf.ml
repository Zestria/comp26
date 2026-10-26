[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open SchonfinkelML_frontend.Common
open Names

type immexpr =
  | ImmNum of int
  | ImmId of string
  | ImmBool of bool
  | ImmUnit

type cexpr =
  | CBinop of
      string
      * immexpr
      * immexpr (* string for now in case of user-defined binary operators *)
  | CImmExpr of immexpr
  | CIfExpr of immexpr * aexpr * aexpr
  | CApp of immexpr * immexpr * immexpr list
  | CAbstr of string * aexpr
  | CMkClosure of string * immexpr list        (* lift_N applied to fewer than arity *)
  | CApplyClosure of immexpr * immexpr list    (* call through variable / unknown *)

and aexpr =
  (* The right-hand side is an aexpr, not a cexpr: a nested let in the
     right-hand side of a recursive binding has to stay in scope of the name. *)
  | ALet of let_flag * string * aexpr * aexpr
  | ACExpr of cexpr

(* A let-binding that has to be emitted before the expression that uses it. *)
type binding = let_flag * string * aexpr

let wrap (bindings : binding list) body =
  List.fold_right (fun (flag, name, rhs) acc -> ALet (flag, name, rhs, acc)) bindings body
;;

let gensym_id naming () = gensym naming ~prefix:"v" ()

let flatten_app left right =
  let rec go f first rest =
    match f with
    | App (g, arg) -> go g arg (first :: rest)
    | _ -> f, first, rest
  in
  go left right []
;;

(* The bindings of an expression together with the cexpr that produces its
   value.  A binding keeps its right-hand side as an aexpr, so nested lets
   never escape the scope of the name they depend on. *)
let rec flatten_aexpr = function
  | ALet (flag, v, rhs, rest) ->
    let bs, c = flatten_aexpr rest in
    (flag, v, rhs) :: bs, c
  | ACExpr c -> [], c
;;

let rec norm_aexpr naming = function
  | Ident x -> naming, ACExpr (CImmExpr (ImmId x))
  | Const (LitInt n) -> naming, ACExpr (CImmExpr (ImmNum n))
  | Const LitUnit -> naming, ACExpr (CImmExpr ImmUnit)
  | App (App (Ident op, e1), e2) when is_binop op ->
    let naming, b1, i1 = norm_imm naming e1 in
    let naming, b2, i2 = norm_imm naming e2 in
    naming, wrap (b1 @ b2) (ACExpr (CBinop (op, i1, i2)))
  | App (left, right) ->
    let f, arg, args = flatten_app left right in
    let naming, bf, if_ = norm_imm naming f in
    let naming, barg, iarg = norm_imm naming arg in
    let naming, bargs, iargs = norm_imm_list naming args in
    naming, wrap (bf @ barg @ bargs) (ACExpr (CApp (if_, iarg, iargs)))
  | If (p, t, e) ->
    let naming, bp, ip = norm_imm naming p in
    let naming, t = norm_aexpr naming t in
    let naming, e = norm_aexpr naming e in
    naming, wrap bp (ACExpr (CIfExpr (ip, t, e)))
  | Abstr (v, body) ->
    let naming, body = norm_aexpr naming body in
    naming, ACExpr (CAbstr (v, body))
  | Let (flag, v, e1, e2) ->
    let naming, rhs = norm_aexpr naming e1 in
    let naming, body = norm_aexpr naming e2 in
    naming, ALet (flag, v, rhs, body)

and norm_imm naming e =
  let naming, a = norm_aexpr naming e in
  let bs, c = flatten_aexpr a in
  match c with
  | CImmExpr i -> naming, bs, i
  | _ ->
    let id, naming = gensym_id naming () in
    naming, bs @ [ NotRecursive, id, ACExpr c ], ImmId id

and norm_imm_list naming = function
  | [] -> naming, [], []
  | e :: rest ->
    let naming, b, i = norm_imm naming e in
    let naming, bs, is = norm_imm_list naming rest in
    naming, b @ bs, i :: is
;;

let create_anf e =
  let naming = naming_of (idents_of_expr StringSet.empty e) in
  snd (norm_aexpr naming e)
;;

let pp_immexpr ppf = function
  | ImmNum n -> Format.fprintf ppf "%d" n
  | ImmId s -> Format.fprintf ppf "%s" s
  | ImmBool b -> Format.fprintf ppf "%b" b
  | ImmUnit -> Format.fprintf ppf "()"
;;

let rec pp_cexpr ppf = function
  | CBinop (op, a, b) -> Format.fprintf ppf "@[%a %s %a@]" pp_immexpr a op pp_immexpr b
  | CImmExpr i -> pp_immexpr ppf i
  | CIfExpr (p, t, e) ->
    Format.fprintf
      ppf
      "@[<2>if %a then@ %a@ else@ %a@]"
      pp_immexpr
      p
      pp_aexpr
      t
      pp_aexpr
      e
  | CApp (f, arg, args) ->
    Format.fprintf
      ppf
      "@[(%a %a%a)@]"
      pp_immexpr
      f
      pp_immexpr
      arg
      (fun ppf -> List.iter (fun a -> Format.fprintf ppf " %a" pp_immexpr a))
      args
  | CAbstr (v, body) -> Format.fprintf ppf "@[<2>fun %s -> %a@]" v pp_aexpr body
  | CMkClosure (f, caps) ->
    Format.fprintf ppf "@[<2>mk_closure %s [%a]@]" f
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ") pp_immexpr)
      caps
  | CApplyClosure (f, args) ->
    Format.fprintf ppf "@[<2>apply %a [%a]@]" pp_immexpr f
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ") pp_immexpr)
      args

and pp_aexpr ppf = function
  | ALet (Recursive, x, rhs, rest) ->
    Format.fprintf ppf "@[<2>let rec %s =@ %a@ in@ %a@]" x pp_aexpr rhs pp_aexpr rest
  | ALet (NotRecursive, x, rhs, rest) ->
    Format.fprintf ppf "@[<2>let %s =@ %a@ in@ %a@]" x pp_aexpr rhs pp_aexpr rest
  | ACExpr c -> pp_cexpr ppf c
;;

module VarSet = Set.Make (String)

let fv_imm : immexpr -> VarSet.t = function
  | ImmId x -> VarSet.singleton x
  | ImmNum _ | ImmBool _ | ImmUnit -> VarSet.empty
;;

let rec fv_cexpr : cexpr -> VarSet.t = function
  | CBinop (_, a, b) -> VarSet.union (fv_imm a) (fv_imm b)
  | CImmExpr i -> fv_imm i
  | CIfExpr (i, t, e) -> VarSet.union (fv_imm i) (VarSet.union (fv_aexpr t) (fv_aexpr e))
  | CApp (f, a, args) ->
    List.fold_left
      (fun acc x -> VarSet.union acc (fv_imm x))
      (VarSet.union (fv_imm f) (fv_imm a))
      args
  | CAbstr (p, body) -> VarSet.remove p (fv_aexpr body)

and fv_aexpr : aexpr -> VarSet.t = function
  | ALet (flag, name, rhs, rest) ->
    let fv_rhs = fv_aexpr rhs in
    let fv_rhs = if flag = Recursive then VarSet.remove name fv_rhs else fv_rhs in
    VarSet.union fv_rhs (VarSet.remove name (fv_aexpr rest))
  | ACExpr c -> fv_cexpr c
;;

let string_of_aexpr a = Format.asprintf "%a" pp_aexpr a
