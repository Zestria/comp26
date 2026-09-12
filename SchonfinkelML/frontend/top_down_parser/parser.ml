[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Recursive descent without a separate lexer: keywords are

    recognised on the parser's demand (as a prefix when [glued]), and
    application never eats an identifier whose keyword prefix terminates
    the current context, so [letx=1inx] reads as [let x = 1 in x] while
    [letter] stays a name (the keyword branch is tried, then rolled back).
    Out of scope: [()], [true]/[false], float, string, [match], [and]. *)

module Ast = Frontend.Common

let ( let* ) = Result.bind

type error = Cursor.error =
  { pos : Lexing.position
  ; msg : string
  }

let pp_error ppf { pos; msg } =
  Format.fprintf ppf "%d:%d: %s" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg
;;

let string_of_error e = Format.asprintf "%a" pp_error e

type ctx =
  { st : Cursor.t
  ; glued : bool
  ; mutable fuel : int
  }

let keywords = [ "let"; "rec"; "in"; "if"; "then"; "else"; "fun" ]
let cmp_ops = [ "<=", Ast.elte; ">=", Ast.egte; "<", Ast.elt; ">", Ast.egt; "=", Ast.eeq ]
let add_ops = [ "+", Ast.eadd; "-", Ast.esub ]
let mul_ops = [ "*", Ast.emul; "/", Ast.ediv ]

(* Backtracking budget: guarantees termination on any input. *)
let max_fuel = 100_000

let is_kw s = List.mem s keywords
let fail ctx msg = Error (Cursor.error ctx.st msg)
let skip ctx = Cursor.skip_ws_and_comments ctx.st

let sub_matches s i pat =
  let l = String.length pat in
  i >= 0 && i + l <= String.length s && String.sub s i l = pat
;;

(* Keywords need a token boundary normally,
   just a prefix when glued. *)
let keyword_ahead ctx kw =
  let st = ctx.st in
  Cursor.starts_with st kw
  && (ctx.glued
      || (match Cursor.peek_at st (String.length kw) with
          | Some c -> not (Cursor.is_ident_char c)
          | None -> true))
;;

let expect_keyword ctx kw =
  if keyword_ahead ctx kw
  then (
    Cursor.advance_n ctx.st (String.length kw);
    Ok ())
  else fail ctx (Printf.sprintf "expected '%s'" kw)
;;

let expect_symbol ctx sym =
  if Cursor.starts_with ctx.st sym
  then (
    Cursor.advance_n ctx.st (String.length sym);
    Ok ())
  else fail ctx (Printf.sprintf "expected '%s'" sym)
;;

(* Operator from the table; '-' must not eat the start of '->'. *)
let op_here ctx ops =
  let st = ctx.st in
  List.find_opt
    (fun (op, _) ->
      Cursor.starts_with st op && not (String.equal op "-" && Cursor.starts_with st "->"))
    ops
;;

(* Identifier run; when glued, cut before the
   nearest follow-set keyword. *)
let ident_run ctx follow =
  let r = Cursor.peek_ident_run ctx.st in
  if (not ctx.glued) || String.equal r ""
  then r
  else (
    let n = String.length r in
    let rec find i =
      if i > n
      then n
      else if List.exists (fun kw -> sub_matches r i kw) follow
      then i
      else find (i + 1)
    in
    String.sub r 0 (find 1))
;;

(* Can an application argument start here? *)
let can_start_argument ctx follow =
  let st = ctx.st in
  match Cursor.peek st with
  | Some c when Cursor.is_digit c -> true
  | Some '(' -> true
  | Some c when Cursor.is_ident_start c ->
    let r = Cursor.peek_ident_run st in
    (* A keyword is never an application argument;
       when glued, also stop at a name whose keyword prefix
       is a follow-set terminator. *)    (not (is_kw r))
    && not (ctx.glued && List.exists (fun kw -> sub_matches r 0 kw) follow)
  | _ -> false
;;

let rec loop_ops ctx follow ops next lhs =
  let* () = skip ctx in
  match op_here ctx ops with
  | None -> Ok lhs
  | Some (op, f) ->
    Cursor.advance_n ctx.st (String.length op);
    let* rhs = next ctx follow in
    loop_ops ctx follow ops next (f lhs rhs)
;;

let desugar_params params body = List.fold_right (fun p acc -> Ast.elambda p acc) params body

let p_binding_name ctx =
  let* () = skip ctx in
  let st = ctx.st in
  match Cursor.peek st with
  | Some c when Cursor.is_ident_start c ->
    let run = Cursor.peek_ident_run st in
    if is_kw run
    then fail ctx (Printf.sprintf "expected an identifier, found keyword '%s'" run)
    else (
      Cursor.advance_n st (String.length run);
      Ok run)
  | _ -> fail ctx "expected an identifier"
;;

(* Names and parameters are read greedily:
   they end at '=' or a space. *)
let p_params ctx =
  let rec loop acc =
    let* () = skip ctx in
    let st = ctx.st in
    match Cursor.peek st with
    | Some c when Cursor.is_ident_start c ->
      let run = Cursor.peek_ident_run st in
      if is_kw run
      then Ok (List.rev acc)
      else (
        Cursor.advance_n st (String.length run);
        loop (run :: acc))
    | _ -> Ok (List.rev acc)
  in
  loop []
;;

let rec p_expr ctx follow =
  let* () = skip ctx in
  if ctx.fuel <= 0
  then fail ctx "expression is too complex"
  else (
    ctx.fuel <- ctx.fuel - 1;
    if ctx.glued
    then (
      let try_keyword kw f =
        if Cursor.starts_with ctx.st kw
        then (
          let snapshot = Cursor.save ctx.st in
          match f ctx follow with
          | Ok v -> Some (Ok v)
          | Error _ ->
            Cursor.restore ctx.st snapshot;
            None)
        else None
      in
      match try_keyword "let" p_let with
      | Some r -> r
      | None ->
        (match try_keyword "if" p_if with
         | Some r -> r
         | None ->
           (match try_keyword "fun" p_fun with
            | Some r -> r
            | None -> p_binop ctx follow)))
    else if keyword_ahead ctx "let"
    then p_let ctx follow
    else if keyword_ahead ctx "if"
    then p_if ctx follow
    else if keyword_ahead ctx "fun"
    then p_fun ctx follow
    else p_binop ctx follow)

and p_let ctx follow =
  let* () = expect_keyword ctx "let" in
  let* () = skip ctx in
  let is_rec =
    if keyword_ahead ctx "rec"
    then (
      Cursor.advance_n ctx.st (String.length "rec");
      true)
    else false
  in
  let* name = p_binding_name ctx in
  let* params = p_params ctx in
  let* () = skip ctx in
  let* () = expect_symbol ctx "=" in
  let* value = p_expr ctx [ "in" ] in
  let* () = expect_keyword ctx "in" in
  let* body = p_expr ctx follow in
  let value = desugar_params params value in
  Ok (if is_rec then Ast.eletrec name value body else Ast.elet name value body)

and p_if ctx follow =
  let* () = expect_keyword ctx "if" in
  let* cond = p_expr ctx [ "then" ] in
  let* () = expect_keyword ctx "then" in
  let* then_branch = p_expr ctx [ "else" ] in
  let* () = expect_keyword ctx "else" in
  let* else_branch = p_expr ctx follow in
  Ok (Ast.eif cond then_branch else_branch)

and p_fun ctx follow =
  let* () = expect_keyword ctx "fun" in
  let* params = p_params ctx in
  if params = []
  then fail ctx "expected a parameter after 'fun'"
  else (
    let* () = skip ctx in
    let* () = expect_symbol ctx "->" in
    let* body = p_expr ctx follow in
    Ok (desugar_params params body))

and p_binop ctx follow = p_cmp ctx follow

and p_cmp ctx follow =
  let* lhs = p_add ctx follow in
  loop_ops ctx follow cmp_ops p_add lhs

and p_add ctx follow =
  let* lhs = p_mul ctx follow in
  loop_ops ctx follow add_ops p_mul lhs

and p_mul ctx follow =
  let* lhs = p_unary ctx follow in
  loop_ops ctx follow mul_ops p_unary lhs

and p_unary ctx follow =
  let* () = skip ctx in
  if Cursor.starts_with ctx.st "-" && not (Cursor.starts_with ctx.st "->")
  then (
    Cursor.advance ctx.st;
    let* e = p_unary ctx follow in
    Ok (Ast.euminus e))
  else p_app ctx follow

and p_app ctx follow =
  let* head = p_atom ctx follow in
  let rec loop acc =
    let* () = skip ctx in
    if can_start_argument ctx follow
    then (
      let* arg = p_atom ctx follow in
      loop (Ast.eapp acc arg))
    else Ok acc
  in
  loop head

and p_atom ctx follow =
  let* () = skip ctx in
  let st = ctx.st in
  match Cursor.peek st with
  | None -> fail ctx "expected an expression"
  | Some c when Cursor.is_digit c ->
    let run = Cursor.peek_int_run st in
    (match int_of_string_opt run with
     | Some n ->
       Cursor.advance_n st (String.length run);
       Ok (Ast.econsti n)
     | None -> fail ctx "integer literal is out of range")
  | Some '(' ->
    Cursor.advance st;
    let* () = skip ctx in
    if Cursor.starts_with st ")"
    then fail ctx "the unit value () is not supported"
    else (
      let* e = p_expr ctx [ ")" ] in
      let* () = skip ctx in
      if Cursor.starts_with st ")"
      then (
        Cursor.advance st;
        Ok e)
      else fail ctx "expected ')'")
  | Some c when Cursor.is_ident_start c ->
    let run = ident_run ctx follow in
    if String.equal run ""
    then fail ctx "expected an expression"
    else if is_kw run
    then fail ctx (Printf.sprintf "unexpected keyword '%s'" run)
    else (
      Cursor.advance_n st (String.length run);
      Ok (Ast.Ident run))
  | Some c -> fail ctx (Printf.sprintf "unexpected character '%c'" c)
;;

let parse_expr ?(glued = false) src =
  let st = Cursor.create src in
  let ctx = { st; glued; fuel = max_fuel } in
  try
    (match p_expr ctx [] with
     | Error e -> Error e
     | Ok e ->
       (match Cursor.skip_ws_and_comments st with
        | Error e -> Error e
        | Ok () ->
          if Cursor.eof st
          then Ok e
          else Error (Cursor.error st "unexpected input after the expression")))
  with
  | Stack_overflow -> Error (Cursor.error st "expression is nested too deeply")
;;
