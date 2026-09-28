[@@@ocaml.text "/*"]

(** Copyright 2026, Kormilcyn, Khmelev and Yakovlev contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* Top-down (combinator) parser built with Angstrom. *)

open Angstrom

let is_space = function
  | ' ' | '\t' | '\n' | '\r' -> true
  | _ -> false
;;

let spaces = skip_while is_space
let spaces1 = take_while1 is_space *> return ()

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_name_start = function
  | 'a' .. 'z' | '_' -> true
  | _ -> false
;;

let is_ident_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
;;

let keywords = [ "if"; "then"; "else"; "let"; "rec"; "in"; "fun" ]

type error = [ `Parsing_error of string ]

let pp_error ppf = function
  | `Parsing_error s -> Format.fprintf ppf "%s" s
;;

let chainl1 p op =
  p
  >>= fun x ->
  many (lift2 (fun f y -> f, y) op p) >>| List.fold_left (fun acc (f, y) -> f acc y) x
;;

(* [let x = e] or [let rec f x = e] without [in]: the beginning of both a
   [let] expression and a top-level declaration. *)
type binding =
  | Value of Ast.name * Ast.expr
  | Recursive of Ast.name * Ast.name * Ast.expr

let let_in binding body =
  match binding with
  | Value (x, e) -> Ast.Let (x, e, body)
  | Recursive (f, p, e) -> Ast.LetRec (f, p, e, body)
;;

let declaration = function
  | Value (x, e) -> Ast.DLet (x, e)
  | Recursive (f, p, e) -> Ast.DLetRec (f, p, e)
;;

let make_parser ~extended =
  let name =
    lift2
      (fun first rest -> String.make 1 first ^ rest)
      (satisfy is_name_start)
      (take_while is_ident_char)
  in
  let reserved s = List.mem s keywords in
  (* [_] can be bound, but it is not a variable. *)
  let identifier =
    name >>= fun s -> if reserved s || String.equal s "_" then fail "keyword" else return s
  in
  let binder_name =
    name >>= fun s -> if (not extended) && reserved s then fail "keyword" else return s
  in
  let head_name =
    binder_name >>= fun s -> if String.equal s "_" then fail "wildcard" else return s
  in
  let integer = take_while1 is_digit >>| int_of_string in
  let keyword s =
    spaces
    *> string s
    *>
    if extended
    then spaces
    else
      peek_char
      >>= function
      | Some c when is_ident_char c -> fail "keyword followed by identifier char"
      | _ -> spaces
  in
  let symbol s = spaces *> string s *> spaces in
  let binding expr =
    let body =
      lift2
        (fun params body -> List.fold_right (fun p b -> Ast.Abs (p, b)) params body)
        (many (spaces1 *> binder_name))
        (symbol "=" *> expr)
    in
    let recursive =
      lift2 (fun f body -> f, body) binder_name body
      >>= function
      | f, Ast.Abs (p, body) -> return (Recursive (f, p, body))
      | _ -> fail "let rec must define a function"
    in
    keyword "let"
    *> (keyword "rec" *> recursive <|> lift2 (fun x e -> Value (x, e)) binder_name body)
  in
  let expr =
    fix (fun expr ->
      (* Arguments are simple, as in OCaml: [f -1] is [f - 1], and [if], [let]
         and [fun] need parentheses. *)
      let simple ~var_name =
        spaces
        *> choice
             [ char '(' *> spaces *> expr <* spaces <* char ')'
             ; (integer >>| fun n -> Ast.Const n)
             ; (var_name >>| fun v -> Ast.Var v)
             ]
        <* spaces
      in
      let atom =
        spaces
        *> choice
             [ (keyword "if" *> expr
                >>= fun cond ->
                keyword "then" *> expr
                >>= fun t1 -> keyword "else" *> expr >>| fun t2 -> Ast.If (cond, t1, t2))
             ; (binding expr >>= fun b -> keyword "in" *> expr >>| let_in b)
             ; simple ~var_name:head_name
             ]
        <* spaces
      in
      let mult_op =
        spaces
        *> choice
             [ string "*" *> return (fun l r -> Ast.BinOp (Ast.Mul, l, r))
             ; string "/" *> return (fun l r -> Ast.BinOp (Ast.Div, l, r))
             ]
        <* spaces
      in
      let add_op =
        spaces
        *> choice
             [ string "+" *> return (fun l r -> Ast.BinOp (Ast.Add, l, r))
             ; string "-" *> return (fun l r -> Ast.BinOp (Ast.Sub, l, r))
             ]
        <* spaces
      in
      let cmp_op =
        spaces
        *> choice
             [ string "<=" *> return (fun l r -> Ast.BinOp (Ast.Le, l, r))
             ; string ">=" *> return (fun l r -> Ast.BinOp (Ast.Ge, l, r))
             ; string "=" *> return (fun l r -> Ast.BinOp (Ast.Eq, l, r))
             ; string "<" *> return (fun l r -> Ast.BinOp (Ast.Lt, l, r))
             ; string ">" *> return (fun l r -> Ast.BinOp (Ast.Gt, l, r))
             ]
        <* spaces
      in
      let app =
        lift2
          (fun head args -> List.fold_left (fun l r -> Ast.App (l, r)) head args)
          atom
          (many (simple ~var_name:identifier))
      in
      let unary =
        fix (fun unary ->
          choice [ (spaces *> char '-' *> unary >>| fun e -> Ast.Neg e); app ])
      in
      let mult = chainl1 unary mult_op in
      let add = chainl1 mult add_op in
      let cmp = chainl1 add cmp_op in
      choice
        [ (keyword "fun"
           *> many1 (spaces *> binder_name <* spaces)
           >>= fun vars ->
           string "->" *> expr
           >>| fun body -> List.fold_right (fun v b -> Ast.Abs (v, b)) vars body)
        ; cmp
        ])
  in
  (* A program may start with an expression; the other items are declarations.
     A [let] followed by [in] is an expression. *)
  let first_item =
    binding expr
    >>= (fun b -> option (declaration b) (keyword "in" *> expr >>| fun e -> Ast.DExpr (let_in b e)))
    <|> (expr >>| fun e -> Ast.DExpr e)
  in
  let program =
    lift2
      (fun first rest -> Option.to_list first @ rest)
      (option None (first_item >>| Option.some))
      (many (binding expr >>| declaration))
  in
  option None (expr >>| Option.some), program >>| Option.some
;;

let syntax_error text offset =
  let line, column =
    String.fold_left
      (fun (line, column) c -> if Char.equal c '\n' then line + 1, 1 else line, column + 1)
      (1, 1)
      (String.sub text 0 offset)
  in
  Printf.sprintf "syntax error at line %d, column %d" line column
;;

(* Parses as much as possible; if something is left, the error points to it. *)
let parse_gen p text =
  let p = lift2 (fun result stop -> result, stop) (spaces *> p <* spaces) pos in
  match parse_string ~consume:Consume.Prefix p text with
  | Ok (Some x, stop) when stop = String.length text -> Ok x
  | Ok (_, stop) -> Error (`Parsing_error (syntax_error text stop))
  | Error msg -> Error (`Parsing_error msg)
;;

let expr_impl, program_impl = make_parser ~extended:false
let expr_impl_extended, program_impl_extended = make_parser ~extended:true
let parse text = parse_gen expr_impl text
let parse_extended text = parse_gen expr_impl_extended text
let parse_program text = parse_gen program_impl text
let parse_program_extended text = parse_gen program_impl_extended text
