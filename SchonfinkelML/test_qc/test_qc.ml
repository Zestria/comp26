[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(* run this test via `dune test  --force` *)

module AST = struct
  type t =
    | Const of (int[@gen QCheck.Gen.return 1])
    | Add of t * t
  [@@deriving qcheck, show { with_path = false }]
end

module PP = struct
  let rec pp ppf = function
    | AST.Const n -> Format.fprintf ppf "%d" n
    (* | Add (l, r) -> Format.fprintf ppf "%a+%a" pp l pp r *)
    | Add (l, r) -> Format.fprintf ppf "(%a+%a)" pp l pp r
  ;;
end

module Parser = struct
  open Angstrom

  let prio expr table =
    let len = Array.length table in
    let rec helper level =
      if level >= len
      then expr
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

  let expr_small =
    let code0 = Char.code '0' in
    Angstrom.satisfy (function
      | '0' .. '9' -> true
      | _ -> false)
    >>| fun c -> AST.Const (Char.code c - code0)
  ;;

  let expr =
    fix (fun self ->
      let add a b = AST.Add (a, b) in
      prio (expr_small <|> (char '(' *> self <* char ')')) [| [ char '+', add ] |])
  ;;
end

let rec shrink_expr =
  let open QCheck.Iter in
  (* fun _ -> empty *)
  function
  | AST.Const _ -> empty
  | Add (l, r) ->
    of_list [ l; r ]
    <+> (shrink_expr l >>= fun l -> return (AST.Add (l, r)))
    <+> (shrink_expr r >>= fun r -> return (AST.Add (l, r)))
;;

let arbitrary_expr =
  (* let open QCheck.Iter in *)
  QCheck.make AST.gen ~print:(Format.asprintf "%a" PP.pp) ~shrink:shrink_expr
;;

module Common_ast = struct
  open Frontend.Common

  let ident_start_gen =
    QCheck.Gen.oneof
      [ QCheck.Gen.map Char.chr (QCheck.Gen.int_range (Char.code 'a') (Char.code 'z'))
      ; QCheck.Gen.map Char.chr (QCheck.Gen.int_range (Char.code 'A') (Char.code 'Z'))
      ]
  ;;

  let ident_cont_gen =
    QCheck.Gen.oneof
      [ ident_start_gen
      ; QCheck.Gen.map Char.chr (QCheck.Gen.int_range (Char.code '0') (Char.code '9'))
      ; QCheck.Gen.return '_'
      ]
  ;;

  let gen_ident =
    QCheck.Gen.(
      ident_start_gen
      >>= fun first ->
      list_size (int_range 0 6) ident_cont_gen
      >>= fun rest ->
      let name = String.of_seq (List.to_seq (first :: rest)) in
      match name with
      | "let" | "in" | "rec" | "if" | "then" | "else" | "fun" -> return (name ^ "_id")
      | _ -> return name)
  ;;

  let gen_literal =
    QCheck.Gen.oneof
      [ QCheck.Gen.map (fun i -> LitInt i) QCheck.Gen.nat_small
      ; QCheck.Gen.return LitUnit
      ]
  ;;

  let gen_let_flag =
    QCheck.Gen.oneof [ QCheck.Gen.return Recursive; QCheck.Gen.return NotRecursive ]
  ;;

  type expr = Frontend.Common.expr =
    | Ident of (string[@gen gen_ident])
    | Const of (literal[@gen gen_literal])
    | App of expr * expr
    | Let of (let_flag[@gen gen_let_flag]) * (string[@gen gen_ident]) * expr * expr
    | If of expr * expr * expr
    | Abstr of (string[@gen gen_ident]) * expr
  [@@deriving qcheck]
end

open Frontend.Common

let rec shrink_common_expr =
  let open QCheck.Iter in
  function
  | Ident _ | Const _ -> empty
  | App (e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink_common_expr e1 >>= fun e1' -> return (App (e1', e2)))
    <+> (shrink_common_expr e2 >>= fun e2' -> return (App (e1, e2')))
  | Let (f, x, e1, e2) ->
    of_list [ e1; e2 ]
    <+> (shrink_common_expr e1 >>= fun e1' -> return (Let (f, x, e1', e2)))
    <+> (shrink_common_expr e2 >>= fun e2' -> return (Let (f, x, e1, e2')))
  | If (c, t, f) ->
    of_list [ c; t; f ]
    <+> (shrink_common_expr c >>= fun c' -> return (If (c', t, f)))
    <+> (shrink_common_expr t >>= fun t' -> return (If (c, t', f)))
    <+> (shrink_common_expr f >>= fun f' -> return (If (c, t, f')))
  | Abstr (v, e) ->
    of_list [ e ] <+> (shrink_common_expr e >>= fun e' -> return (Abstr (v, e')))
;;

let arbitrary_common_expr =
  QCheck.make
    (QCheck.Gen.sized (fun size -> Common_ast.gen_expr_sized (min (size / 2) 5)))
    ~print:Frontend.Common.string_of_expr
    ~shrink:shrink_common_expr
;;

let parse_common s =
  Frontend.Bottom_up_parser.program Frontend.Lexer.token (Lexing.from_string s)
;;

let _ =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_expr (fun l ->
          match
            Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              Parser.expr
              (Format.asprintf "%a" PP.pp l)
          with
          | Result.Ok after when after = l -> true
          | Result.Ok after ->
            Format.printf "before : %a\n%!" AST.pp l;
            (* Format.printf "       : `%a`\n%!" PP.pp l; *)
            Format.printf "`%a`\n%!" AST.pp after;
            false
          | Result.Error _ ->
            (* Format.printf "failed on : %a\n%!" Lam.pp l; *)
            false))
    ; QCheck.(
        Test.make arbitrary_common_expr (fun e ->
          let s = Frontend.Common.string_of_expr e in
          try parse_common s = e with
          | _ -> false))
    ]
;;
