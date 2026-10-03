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

and cexpr =
  | CBinop of
      string
      * immexpr
      * immexpr (* string for now in case of user-defined binary operators *)
  | CImmExpr of immexpr
  | CIfExpr of immexpr * aexpr * aexpr
  | CApp of immexpr * immexpr * immexpr list
  | CAbstr of string * cexpr

and aexpr =
  | ALet of let_flag * string * cexpr * aexpr
  | ACExpr of cexpr

let aexpr_of_immexpr imm = ACExpr (CImmExpr imm)

let gensym =
  let n = ref 0 in
  fun () ->
    incr n;
    !n
;;

let gensym_s =
  fun ?(prefix = "temp") () ->
  let n = gensym () in
  Printf.sprintf "%s%d" prefix n
;;

let gensym_id ?(prefix = "v") () = gensym_s ~prefix ()

let create_anf expr =
  let rec aux e (k : immexpr -> aexpr) =
    match e with
    | Ident x -> k (ImmId x)
    | Const (LitInt n) -> k (ImmNum n)
    | Const LitUnit -> k ImmUnit
    | App (App (Ident s, left), right) when is_binop s ->
      let id = gensym_id () in
      aux left (fun limm ->
        aux right (fun rimm ->
          ALet (NotRecursive, id, CBinop (s, limm, rimm), k (ImmId id))))
    | App (left, right) ->
      let rec unwrap expr acc aux_expr =
        match expr with
        | App (new_left, new_right) -> unwrap new_left (aux_expr :: acc) new_right
        | expr -> expr, aux_expr, acc
      in
      let left, right, tail = unwrap left [] right in
      let rec fold_aux h1 h2 acc = function
        | [] -> ACExpr (CApp (h1, h2, List.rev acc))
        | x :: xs -> aux x (fun imm -> fold_aux h1 h2 (imm :: acc) xs)
      in
      aux left (fun limm -> aux right (fun rimm -> fold_aux limm rimm [] tail))
    | Let (flag, v, left, right) ->
      aux left (fun limm -> ALet (flag, v, CImmExpr limm, aux right k))
    | Abstr (v, e) -> aux e (fun imm -> ACExpr (CAbstr (v, CImmExpr imm)))
    | If (p, t, e) ->
      aux p (fun imm ->
        ACExpr (CIfExpr (imm, aux t aexpr_of_immexpr, aux e aexpr_of_immexpr)))
  in
  aux expr aexpr_of_immexpr
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
  | CAbstr (v, body) -> Format.fprintf ppf "@[<2>fun %s -> %a@]" v pp_cexpr body

and pp_aexpr ppf = function
  | ALet (Recursive, x, c, rest) ->
    Format.fprintf ppf "@[<2>let rec %s =@ %a@ in@ %a@]" x pp_cexpr c pp_aexpr rest
  | ALet (NotRecursive, x, c, rest) ->
    Format.fprintf ppf "@[<2>let %s =@ %a@ in@ %a@]" x pp_cexpr c pp_aexpr rest
  | ACExpr c -> pp_cexpr ppf c
;;

let string_of_aexpr a = Format.asprintf "%a" pp_aexpr a
