[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type t =
  { src : string
  ; mutable pos : int
  ; mutable lnum : int
  ; mutable bol : int
  }

type state =
  { spos : int
  ; slnum : int
  ; sbol : int
  }

type error =
  { pos : Lexing.position
  ; msg : string
  }

let create src = { src; pos = 0; lnum = 1; bol = 0 }
let source t = t.src
let offset (t : t) = t.pos

let position t =
  { Lexing.pos_fname = ""; pos_lnum = t.lnum; pos_bol = t.bol; pos_cnum = t.pos }
;;

let error t msg = { pos = position t; msg }
let save (t : t) = { spos = t.pos; slnum = t.lnum; sbol = t.bol }

let restore (t : t) st =
  t.pos <- st.spos;
  t.lnum <- st.slnum;
  t.bol <- st.sbol
;;

let eof (t : t) = t.pos >= String.length t.src
let peek (t : t) = if eof t then None else Some t.src.[t.pos]

let peek_at (t : t) n =
  let i = t.pos + n in
  if i >= 0 && i < String.length t.src then Some t.src.[i] else None
;;

let advance t =
  if not (eof t)
  then (
    if t.src.[t.pos] = '\n'
    then (
      t.lnum <- t.lnum + 1;
      t.bol <- t.pos + 1);
    t.pos <- t.pos + 1)
;;

let advance_n (t : t) n =
  for _ = 1 to n do
    advance t
  done
;;

let is_digit c = c >= '0' && c <= '9'
let is_lower c = c >= 'a' && c <= 'z'
let is_upper c = c >= 'A' && c <= 'Z'
let is_letter c = is_lower c || is_upper c
let is_ident_start = is_letter
let is_ident_char c = is_letter c || is_digit c || c = '_' || c = '\''

let starts_with (t : t) s =
  let n = String.length s in
  t.pos + n <= String.length t.src && String.sub t.src t.pos n = s
;;

let peek_ident_run t =
  if eof t || not (is_letter t.src.[t.pos])
  then ""
  else (
    let n = String.length t.src in
    let i = ref t.pos in
    while !i < n && is_ident_char t.src.[!i] do
      incr i
    done;
    String.sub t.src t.pos (!i - t.pos))
;;

let peek_int_run t =
  if eof t || not (is_digit t.src.[t.pos])
  then ""
  else (
    let n = String.length t.src in
    let i = ref t.pos in
    while !i < n && is_digit t.src.[!i] do
      incr i
    done;
    String.sub t.src t.pos (!i - t.pos))
;;

let skip_ws_and_comments t =
  let rec skip () =
    match peek t with
    | Some (' ' | '\t' | '\n' | '\r') ->
      advance t;
      skip ()
    | Some '(' when starts_with t "(*" ->
      advance t;
      advance t;
      comment 0
    | _ -> Ok ()
  and comment depth =
    if eof t
    then Error (error t "unclosed comment")
    else if starts_with t "(*"
    then (
      advance t;
      advance t;
      comment (depth + 1))
    else if starts_with t "*)"
    then (
      advance t;
      advance t;
      if depth = 0 then skip () else comment (depth - 1))
    else (
      advance t;
      comment depth)
  in
  skip ()
;;
