[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Ast = Frontend.Common
module Lexer = Frontend.Lexer

let ( let* ) = Result.bind

(** The parser reports the errors of the lexer as they are: one [pos] and one
   [msg] is all either of them has to say. *)
type error = Lexer.error

(** [Lexer.pp_error] calls every message it prints a lexing error, and a source
   that is merely unparsable is not one; the position is spelled out here
   instead, so that the two of them read alike up to that word. *)
let pp_error ppf ({ pos; msg } : error) =
  Format.fprintf ppf "%d:%d: %s" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg
;;

let string_of_error e = Format.asprintf "%a" pp_error e

let tokenize src =
  let lexbuf = Lexing.from_string src in
  let rec loop acc =
    let tok = Lexer.token lexbuf in
    let pos = lexbuf.Lexing.lex_start_p in
    match tok with
    | Ast.EOF -> Ok (List.rev ((tok, pos) :: acc))
    | _ -> loop ((tok, pos) :: acc)
  in
  try loop [] with
  | Lexer.Lexing_error e -> Error e
;;

type ctx =
  { toks : (Ast.token * Lexing.position) array
  ; ops : Operators.t
  ; mutable pos : int
  ; mutable budget : int
  }

let default_budget = 100_000
let cur ctx = fst ctx.toks.(ctx.pos)
let cur_pos ctx = snd ctx.toks.(ctx.pos)

(** [tokenize] always ends with [EOF] and [advance] never moves past it, so
   [ctx.pos] is always a valid index. *)
let advance ctx = if ctx.pos < Array.length ctx.toks - 1 then ctx.pos <- ctx.pos + 1

let fail ctx msg : (_, error) result = Error { pos = cur_pos ctx; msg }

let expect ctx tok what =
  if cur ctx = tok
  then (
    advance ctx;
    Ok ())
  else fail ctx (Printf.sprintf "expected '%s'" what)
;;

let is_keyword = function
  | Ast.LET | Ast.REC | Ast.IN | Ast.IF | Ast.THEN | Ast.ELSE | Ast.FUN -> true
  | _ -> false
;;

let pp_tok tok = Format.asprintf "%a" Ast.pp_token tok

let expect_ident ctx =
  match cur ctx with
  | Ast.IDENT name ->
    advance ctx;
    Ok name
  | tok ->
    if is_keyword tok
    then fail ctx (Printf.sprintf "expected an identifier, found keyword '%s'" (pp_tok tok))
    else fail ctx "expected an identifier"
;;

(** Names and parameters are read greedily: they end at the first token that is
   not an identifier. *)
let p_params ctx =
  let rec p_param acc =
    match cur ctx with
    | Ast.IDENT name ->
      advance ctx;
      p_param (name :: acc)
    | _ -> List.rev acc
  in
  p_param []
;;

let desugar_params params body =
  List.fold_right (fun p acc -> Ast.elambda p acc) params body
;;

(* The precondition of the right hand side of [e]: a left associative operator
   gives up equality, a right associative one does not. *)
let rhs_min_prec e =
  match e.Operators.fixity with
  | Operators.Infix Operators.Right -> e.Operators.prec
  | _ -> e.Operators.prec + 1
;;

let rec p_expr ctx =
  if ctx.budget <= 0
  then fail ctx "expression is too complex"
  else (
    ctx.budget <- ctx.budget - 1;
    match cur ctx with
    | Ast.LET -> p_let ctx
    | Ast.IF -> p_if ctx
    | Ast.FUN -> p_fun ctx
    | _ -> p_binop ctx 0)

(** Precedence climbing: [p_binop ctx min_prec] reads an operand followed by all
   the operators of the table that bind at least as tight as [min_prec]. *)
and p_binop ctx min_prec =
  let* lhs = p_operand ctx in
  p_infix ctx min_prec lhs

and p_infix ctx min_prec lhs =
  match Operators.find_infix ctx.ops (cur ctx) with
  | Some e when e.Operators.prec >= min_prec ->
    advance ctx;
    let* rhs = p_binop ctx (rhs_min_prec e) in
    let* () = check_chaining ctx e in
    p_infix ctx min_prec (Ast.eapp (Ast.eapp (Ast.Ident e.Operators.sym) lhs) rhs)
  | _ -> Ok lhs

(** A non-associative operator cannot be followed by an operator of the same
   precedence, so [a = b = c] is rejected rather than silently associated. *)
(* MAYBE EASIER *)
and check_chaining ctx e =
  match e.Operators.fixity with
  | Operators.Infix Operators.Nonassoc ->
    (match Operators.find_infix ctx.ops (cur ctx) with
     | Some next when next.Operators.prec = e.Operators.prec ->
       if next.Operators.sym = e.Operators.sym
       then fail ctx (Printf.sprintf "operator '%s' is not associative" e.Operators.sym)
       else
         fail
           ctx
           (Printf.sprintf
              "operators '%s' and '%s' are not associative"
              e.Operators.sym
              next.Operators.sym)
     | _ -> Ok ())
  | _ -> Ok ()

and p_operand ctx =
  match Operators.find_prefix ctx.ops (cur ctx) with
  | Some e ->
    advance ctx;
    let* arg = p_binop ctx e.Operators.prec in
    Ok (Ast.eapp (Ast.Ident e.Operators.sym) arg)
  | None -> p_app ctx

and p_app ctx =
  let* head = p_atom ctx in
  let rec p_arg acc =
    match cur ctx with
    | Ast.INT _ | Ast.IDENT _ | Ast.LPAREN ->
      let* arg = p_atom ctx in
      p_arg (Ast.eapp acc arg)
    | _ -> Ok acc
  in
  p_arg head

and p_atom ctx =
  match cur ctx with
  | Ast.INT n ->
    advance ctx;
    Ok (Ast.econsti n)
  | Ast.IDENT name ->
    advance ctx;
    Ok (Ast.Ident name)
  | Ast.LPAREN ->
    advance ctx;
    if cur ctx = Ast.RPAREN
    then fail ctx "the unit value () is not supported"
    else
      let* e = p_expr ctx in
      let* () = expect ctx Ast.RPAREN ")" in
      Ok e
  | Ast.UNIT -> fail ctx "the unit value () is not supported"
  | Ast.EOF -> fail ctx "expected an expression"
  | tok ->
    let what = if is_keyword tok then "keyword" else "token" in
    fail ctx (Printf.sprintf "unexpected %s '%s'" what (pp_tok tok))

and p_let ctx =
  let* () = expect ctx Ast.LET "let" in
  let is_rec =
    if cur ctx = Ast.REC
    then (
      advance ctx;
      true)
    else false
  in
  let* name = expect_ident ctx in
  let params = p_params ctx in
  let* () = expect ctx Ast.EQ "=" in
  let* value = p_expr ctx in
  let* () = expect ctx Ast.IN "in" in
  let* body = p_expr ctx in
  let value = desugar_params params value in
  Ok (if is_rec then Ast.eletrec name value body else Ast.elet name value body)

and p_if ctx =
  let* () = expect ctx Ast.IF "if" in
  let* cond = p_expr ctx in
  let* () = expect ctx Ast.THEN "then" in
  let* then_branch = p_expr ctx in
  let* () = expect ctx Ast.ELSE "else" in
  let* else_branch = p_expr ctx in
  Ok (Ast.eif cond then_branch else_branch)

and p_fun ctx =
  let* () = expect ctx Ast.FUN "fun" in
  let params = p_params ctx in
  if params = []
  then fail ctx "expected a parameter after 'fun'"
  else
    let* () = expect ctx Ast.RIGHTARROW "->" in
    let* body = p_expr ctx in
    Ok (desugar_params params body)
;;

let parse_expr ?(ops = Operators.default) ?(max_budget = default_budget) src =
  match tokenize src with
  | Error e -> Error e
  | Ok toks ->
    let ctx = { toks = Array.of_list toks; ops; pos = 0; budget = max_budget } in
    (try
       let* e = p_expr ctx in
       if cur ctx = Ast.EOF
       then Ok e
       else fail ctx "unexpected input after the expression"
     with
     | Stack_overflow -> fail ctx "expression is nested too deeply")
;;
