[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Frontend.Common

type binop =
  | Plus
  | Minus
  | Mul
  | Divide
  | Equal
  | GreaterThan
  | LessThan
  | GreaterThanEqual
  | LessThanEqual

let binop_of_string = function
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Mul
  | "/" -> Divide
  | "=" -> Equal
  | "<" -> GreaterThan
  | ">" -> LessThan
  | "<=" -> GreaterThanEqual
  | ">=" -> LessThanEqual
  | _ -> failwith "Not a binary operation"
;;

type immexpr =
  | ImmNum of int
  | ImmId of string

and cexpr =
  | CBinop of binop * immexpr * immexpr
  | CImmExpr of immexpr

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
    | App (App (Ident s, left), right) when is_binop s ->
      let id = gensym_id () in
      let binop = binop_of_string s in
      aux left (fun limm ->
        aux right (fun rimm ->
          ALet (NotRecursive, id, CBinop (binop, limm, rimm), k (ImmId id))))
    | App _ -> assert false
    | Let _ -> assert false
    | If _ -> assert false
    | Abstr _ -> assert false
    | _ -> assert false
  in
  aux expr aexpr_of_immexpr
;;
