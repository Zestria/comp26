[@@@ocaml.text "/*"]

(** Copyright 2026, Dmitrii Kuznetsov and other contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Recursive-descent grammar over pure State_error parser combinators.
    Sticky keyword mode and the precedence ladder are unchanged. Each parse
    has its own cursor and diagnostics; no global error accumulator exists. *)
open Parser_state

(* Character classes -------------------------------------------------- *)

let is_ws = function
  | ' ' | '\t' | '\r' | '\n' -> true
  | _ -> false
;;

let is_id_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

(* Whitespace and comments -------------------------------------------- *)

(** Non-nested comments: an opening ["(*"] up to a closing ["*)"].

    [fix] is used instead of [let rec ... ()] so that the recursion is
    lazy: combinators like [*>] are ordinary functions whose arguments are
    evaluated eagerly, so a self-call in an argument position would loop
    forever at parser construction time.  The fix point is the inner loop
    only (after the opening ["(*"]). *)
let comment =
  string "(*"
  *> fix (fun loop ->
    peek_char
    >>= function
    | None -> fail_at "unterminated comment"
    | Some '*' ->
      tracked (advance 1)
      >>= fun () ->
      peek_char
      >>= (function
       | Some ')' -> tracked (advance 1) *> return ()
       | _ -> loop)
    | Some _ -> tracked (advance 1) *> loop)
;;

(** Optional top-level separator [";;"] (phrase terminator, matching the
    [SEMI] token of the bottom-up grammar).  It is consumed between
    declarations (and after the last one) by {!program}, but NOT inside
    expressions: an application never glues across a [";;"], so separate
    declarations stay separate. *)
let semi = tracked (string ";;")

let skip_ws =
  fix (fun skip_ws ->
    tracked (skip_many (satisfy is_ws) *> (comment *> skip_ws <|> return ())))
;;

(* Tokens -------------------------------------------------------------- *)

(** Keyword match.

    - Non-sticky (default): the keyword must not be a prefix of a longer
      identifier ([letx] is a variable, not [let] followed by [x]).
    - Sticky: the keyword wins at the character level; the word-boundary
      check is dropped, so [letx] splits into [let] + [x]. *)
let kw ~sticky s =
  tracked (string s)
  >>= fun _ ->
  if sticky
  then return ()
  else
    peek_char
    >>= function
    | Some c when is_id_char c ->
      fail_at (s ^ " is a keyword and cannot start an identifier")
    | _ -> return ()
;;

let keyword ~sticky s = skip_ws *> kw ~sticky s
let sym s = skip_ws *> tracked (string s)
let keywords = [ "let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "mod"; "true"; "false" ]

(** Identifier.  The bare keywords are rejected in both modes.  In sticky
    mode the flag only controls *where* identifiers may be reached: a
    keyword match wins first in expression position, while a keyword-prefixed
    name like [letx] is still a valid identifier wherever {!ident} is used
    directly (binding names, parameters). *)
let ident =
  tracked (take_while1 is_id_char)
  >>= fun s ->
  if List.mem s keywords
  then fail_at ("keyword " ^ s ^ " cannot be used as an identifier")
  else return s
;;

let int_lit =
  tracked (take_while1 is_digit)
  >>= fun s ->
  match int_of_string_opt s with
  | Some n -> return n
  | None -> fail_at "integer literal is out of range"
;;

let plus_op = sym "+"
let minus_op = sym "-"
let star_op = sym "*"
let slash_op = sym "/"
let mod_op ~sticky = keyword ~sticky "mod" *> return "mod"
let eq_op = sym "="
let neq_op = sym "<>"
let lt_op = sym "<"
let le_op = sym "<="
let gt_op = sym ">"
let ge_op = sym ">="

(* Precedence ladder --------------------------------------------------- *)

(** Left-associative chain over a table of operator levels; the first
    level has the lowest precedence. *)
let prio base table =
  let rec level = function
    | [] -> base
    | operators :: rest ->
      let operand = level rest in
      return (List.fold_left (fun acc (op, right) -> op acc right))
      <*> operand
      <*> many
            (choice
               (List.map
                  (fun (op, combine) ->
                     op *> operand >>= fun right -> return (combine, right))
                  operators))
  in
  level table
;;

(* Program ------------------------------------------------------------- *)

let program ~sticky =
  skip_ws
  *> many
       (fix (fun expr ->
          let parens = sym "(" *> expr <* skip_ws <* tracked (char ')') in
          let atom =
            skip_ws
            *> choice
                 [ (int_lit >>| fun n -> Ast.Int n)
                 ; keyword ~sticky "true" *> return (Ast.Bool true)
                 ; keyword ~sticky "false" *> return (Ast.Bool false)
                 ; (ident >>| fun x -> Ast.Var x)
                 ; parens
                 ]
          in
          let app =
            atom
            >>= fun f ->
            many atom
            >>= fun args -> return (List.fold_left (fun acc a -> Ast.App (acc, a)) f args)
          in
          let unary =
            fix (fun unary ->
              skip_ws *> tracked (char '-') *> unary >>| (fun e -> Ast.Neg e) <|> app)
          in
          let binop =
            prio
              unary
              [ [ (eq_op, fun l r -> Ast.Binop (Ast.Eq, l, r))
                ; (neq_op, fun l r -> Ast.Binop (Ast.Neq, l, r))
                ; (lt_op, fun l r -> Ast.Binop (Ast.Lt, l, r))
                ; (le_op, fun l r -> Ast.Binop (Ast.Le, l, r))
                ; (gt_op, fun l r -> Ast.Binop (Ast.Gt, l, r))
                ; (ge_op, fun l r -> Ast.Binop (Ast.Ge, l, r))
                ]
              ; [ (plus_op, fun l r -> Ast.Binop (Ast.Add, l, r))
                ; (minus_op, fun l r -> Ast.Binop (Ast.Sub, l, r))
                ]
              ; [ (star_op, fun l r -> Ast.Binop (Ast.Mul, l, r))
                ; (slash_op, fun l r -> Ast.Binop (Ast.Div, l, r))
                ; (mod_op ~sticky, fun l r -> Ast.Binop (Ast.Mod, l, r))
                ]
              ]
          in
          let let_parser =
            keyword ~sticky "let" *> option false (keyword ~sticky "rec" *> return true)
            >>= fun rec_ ->
            skip_ws *> ident
            >>= fun name ->
            many (skip_ws *> ident)
            >>= fun params ->
            sym "=" *> expr
            >>= fun body ->
            option None (keyword ~sticky "in" *> expr >>| fun e -> Some e)
            >>= fun cont -> return (Ast.Let ({ Ast.rec_; name; params; body }, cont))
          in
          let fun_parser =
            keyword ~sticky "fun" *> many1 (skip_ws *> ident)
            >>= fun params -> sym "->" *> expr >>| fun body -> Ast.Fun (params, body)
          in
          let if_parser =
            keyword ~sticky "if" *> expr
            >>= fun cond ->
            keyword ~sticky "then" *> expr
            >>= fun thn ->
            keyword ~sticky "else" *> expr >>| fun els -> Ast.If (cond, thn, els)
          in
          let item = choice [ let_parser; fun_parser; if_parser; binop ] in
          item <* (semi *> return () <|> return ())))
  <* skip_ws
;;

let parse_prog ~sticky source = Parser_state.run (program ~sticky) source
