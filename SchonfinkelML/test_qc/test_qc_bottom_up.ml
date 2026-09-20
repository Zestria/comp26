[@@@ocaml.text "/*"]

(** Copyright 2023-2025, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

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

let check_common_expr e =
  let s = "let x = " ^ Frontend.Common.string_of_expr e in
  match parse_common s with
  | [ StrLet (NotRecursive, "x", e') ] -> e = e'
  | _ -> false
;;

let _ =
  QCheck_runner.run_tests
    ~verbose:true
    [ QCheck.(
        Test.make
          ~name:"bottom-up round-trip (wrapped in let)"
          ~count:1000
          arbitrary_common_expr
          check_common_expr)
    ]
;;
