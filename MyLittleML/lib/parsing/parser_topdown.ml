(** Top-down parser for the MyLittleML OCaml subset, built with Angstrom.

    Produces the same {!Ast.prog} as the menhir-based bottom-up parser
    ([parser.mly]): integers, booleans, variables, application, anonymous
    functions, [let] bindings with an optional [in] continuation,
    [if-then-else], a precedence ladder of binary operators and unary
    minus, parentheses and comments.  Whitespace handling (including
    comments) is done here with Angstrom combinators and shares no code
    with [lexer.mll].

    Angstrom backtracking is the default: a failed alternative rewinds the
    input, which lets the keyword parsers below require a word boundary
    ([letx] is an identifier, not [let] followed by [x]) while still
    falling back to the identifier rule.

    Precedence (low -> high): comparisons -> [+ -] -> [* / mod] ->
    unary minus -> application. *)

open Angstrom

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
         >>= (function
           | None -> fail "unterminated comment"
           | Some '*' ->
             advance 1
             >>= fun () ->
             peek_char
             >>= (function
               | Some ')' -> advance 1 *> return ()
               | _ -> advance 1 *> loop)
           | Some _ -> advance 1 *> loop))
;;

let skip_ws =
  fix (fun skip_ws ->
      skip_many (satisfy is_ws) *> (comment *> skip_ws <|> return ()))
;;

(* Tokens -------------------------------------------------------------- *)

(** Keyword that must not be a prefix of a longer identifier. *)
let kw s =
  string s
  *> peek_char
  >>= (function
    | Some c when is_id_char c -> fail (s ^ " is a keyword and cannot start an identifier")
    | _ -> return ())
;;

let keyword s = skip_ws *> kw s

let sym s = skip_ws *> string s

let keywords =
  [ "let"; "rec"; "in"; "fun"; "if"; "then"; "else"; "mod"; "true"; "false" ]
;;

let ident =
  take_while1 is_id_char
  >>= fun s ->
  if List.mem s keywords
  then fail ("keyword " ^ s ^ " cannot be used as an identifier")
  else return s
;;

let int_lit =
  take_while1 is_digit
  >>= fun s ->
  match int_of_string_opt s with
  | Some n -> return n
  | None -> fail "integer literal is out of range"
;;

let plus_op = sym "+"
let minus_op = sym "-"
let star_op = sym "*"
let slash_op = sym "/"
let mod_op = keyword "mod" *> return "mod"
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
  let len = Array.length table in
  let rec helper level =
    if level >= len
    then base
    else (
      let xs = table.(level) in
      return (List.fold_left (fun acc (op, r) -> op acc r))
      <*> helper (level + 1)
      <*> many
            (choice
               (List.map
                  (fun (op, f) -> op *> helper (level + 1) >>= fun r -> return (f, r))
                  xs)))
  in
  helper 0
;;

(* Program ------------------------------------------------------------- *)

let program =
  skip_ws
  *> many
       (fix (fun expr ->
            let parens = sym "(" *> expr <* skip_ws <* char ')' in
            let atom =
              skip_ws
              *> choice
                   [ (int_lit >>| fun n -> Ast.Int n)
                   ; (keyword "true" *> return (Ast.Bool true))
                   ; (keyword "false" *> return (Ast.Bool false))
                   ; (ident >>| fun x -> Ast.Var x)
                   ; parens ]
            in
            let app =
              atom
              >>= fun f ->
              many atom
              >>= fun args ->
              return (List.fold_left (fun acc a -> Ast.App (acc, a)) f args)
            in
            let unary =
              fix (fun unary ->
                  (skip_ws *> char '-' *> unary >>| fun e -> Ast.Neg e)
                  <|> app)
            in
            let binop =
              prio unary
                [| [ (eq_op, fun l r -> Ast.Binop (Ast.Eq, l, r))
                   ; (neq_op, fun l r -> Ast.Binop (Ast.Neq, l, r))
                   ; (lt_op, fun l r -> Ast.Binop (Ast.Lt, l, r))
                   ; (le_op, fun l r -> Ast.Binop (Ast.Le, l, r))
                   ; (gt_op, fun l r -> Ast.Binop (Ast.Gt, l, r))
                   ; (ge_op, fun l r -> Ast.Binop (Ast.Ge, l, r)) ]
                 ; [ (plus_op, fun l r -> Ast.Binop (Ast.Add, l, r))
                   ; (minus_op, fun l r -> Ast.Binop (Ast.Sub, l, r)) ]
                 ; [ (star_op, fun l r -> Ast.Binop (Ast.Mul, l, r))
                   ; (slash_op, fun l r -> Ast.Binop (Ast.Div, l, r))
                   ; (mod_op, fun l r -> Ast.Binop (Ast.Mod, l, r)) ] |]
            in
            let let_parser =
              keyword "let"
              *> option false (keyword "rec" *> return true)
              >>= fun rec_ ->
              skip_ws *> ident
              >>= fun name ->
              many (skip_ws *> ident)
              >>= fun params ->
              sym "=" *> expr
              >>= fun body ->
              option None (keyword "in" *> expr >>| fun e -> Some e)
              >>= fun cont ->
              return (Ast.Let ({ Ast.rec_; name; params; body }, cont))
            in
            let fun_parser =
              keyword "fun"
              *> many1 (skip_ws *> ident)
              >>= fun params ->
              sym "->" *> expr >>| fun body -> Ast.Fun (params, body)
            in
            let if_parser =
              keyword "if" *> expr
              >>= fun cond ->
              keyword "then" *> expr
              >>= fun thn ->
              keyword "else" *> expr >>| fun els -> Ast.If (cond, thn, els)
            in
            choice [ let_parser; fun_parser; if_parser; binop ]))
  <* skip_ws
;;

let parse_prog source =
  match parse_string ~consume:Consume.All program source with
  | Ok prog -> Ok prog
  | Error msg -> Error msg
;;