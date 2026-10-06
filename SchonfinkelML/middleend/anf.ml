[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Common

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

module StringSet = Set.Make (String)

let rec idents_of_expr acc = function
  | Ident x -> StringSet.add x acc
  | Const _ -> acc
  | App (e1, e2) -> idents_of_expr (idents_of_expr acc e1) e2
  | Let (_, v, e1, e2) -> idents_of_expr (idents_of_expr (StringSet.add v acc) e1) e2
  | If (e1, e2, e3) -> idents_of_expr (idents_of_expr (idents_of_expr acc e1) e2) e3
  | Abstr (v, e) -> idents_of_expr (StringSet.add v acc) e
;;

(* Fresh names must not collide with names that occur in the source.  The
   supply is threaded through the conversion instead of living in mutable
   cells, so [create_anf] stays a pure function. *)
type naming =
  { reserved : StringSet.t
  ; counter : int
  }

let naming_of idents = { reserved = idents; counter = 0 }

let gensym (naming : naming) ?(prefix = "temp") () =
  let rec pick counter =
    let name = Printf.sprintf "%s%d" prefix counter in
    if StringSet.mem name naming.reserved then pick (counter + 1) else name, counter
  in
  let name, counter = pick (naming.counter + 1) in
  name, { naming with counter }
;;

let gensym_id naming () = gensym naming ~prefix:"v" ()

let fv_imm : immexpr -> StringSet.t = function
  | ImmId x -> StringSet.singleton x
  | ImmNum _ | ImmBool _ | ImmUnit -> StringSet.empty
;;

let rec fv_cexpr : cexpr -> StringSet.t = function
  | CBinop (_, a, b) -> StringSet.union (fv_imm a) (fv_imm b)
  | CImmExpr i -> fv_imm i
  | CIfExpr (i, t, e) ->
    StringSet.union (fv_imm i) (StringSet.union (fv_aexpr t) (fv_aexpr e))
  | CApp (f, a, args) ->
    List.fold_left
      (fun acc x -> StringSet.union acc (fv_imm x))
      (StringSet.union (fv_imm f) (fv_imm a))
      args
  | CAbstr (p, body) -> StringSet.remove p (fv_aexpr body)

and fv_aexpr : aexpr -> StringSet.t = function
  | ALet (flag, name, rhs, rest) ->
    let fv_rhs = fv_aexpr rhs in
    let fv_rhs = if flag = Recursive then StringSet.remove name fv_rhs else fv_rhs in
    StringSet.union fv_rhs (StringSet.remove name (fv_aexpr rest))
  | ACExpr c -> fv_cexpr c
;;

let free_vars a = StringSet.elements (fv_aexpr a)

let rec idents_cexpr acc : cexpr -> StringSet.t = function
  | CBinop (_, a, b) -> idents_imm (idents_imm acc a) b
  | CImmExpr i -> idents_imm acc i
  | CIfExpr (i, t, e) -> idents_aexpr (idents_aexpr (idents_imm acc i) t) e
  | CApp (f, a, args) -> List.fold_left idents_imm (idents_imm (idents_imm acc f) a) args
  | CAbstr (p, body) -> idents_aexpr (StringSet.add p acc) body

and idents_imm acc : immexpr -> StringSet.t = function
  | ImmId x -> StringSet.add x acc
  | ImmNum _ | ImmBool _ | ImmUnit -> acc

and idents_aexpr acc : aexpr -> StringSet.t = function
  | ALet (_, name, rhs, rest) ->
    idents_aexpr (idents_aexpr (StringSet.add name acc) rhs) rest
  | ACExpr c -> idents_cexpr acc c
;;

let all_idents a = idents_aexpr StringSet.empty a
let naming_of_aexpr a = naming_of (all_idents a)

let rec collect_params = function
  | ACExpr (CAbstr (p, body)) ->
    let ps, inner = collect_params body in
    p :: ps, inner
  | other -> [], other
;;

let rec build_abstr params body =
  match params with
  | [] -> body
  | [ p ] -> ACExpr (CAbstr (p, body))
  | p :: rest -> ACExpr (CAbstr (p, build_abstr rest body))
;;

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

and pp_aexpr ppf = function
  | ALet (Recursive, x, rhs, rest) ->
    Format.fprintf ppf "@[<2>let rec %s =@ %a@ in@ %a@]" x pp_aexpr rhs pp_aexpr rest
  | ALet (NotRecursive, x, rhs, rest) ->
    Format.fprintf ppf "@[<2>let %s =@ %a@ in@ %a@]" x pp_aexpr rhs pp_aexpr rest
  | ACExpr c -> pp_cexpr ppf c
;;

let string_of_aexpr a = Format.asprintf "%a" pp_aexpr a
