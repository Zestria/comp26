[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type parser =
  { name : string
  ; parse : string -> (Ast.structure, string) result
  }

let top_down : parser = { name = "top-down"; parse = Rd_parser.parse_structure }
let bottom_up : parser = { name = "menhir"; parse = Menhir_parser.parse_structure }

let rec shrink_pattern p =
  let open QCheck.Iter in
  match p with
  | Ast.PAny | Ast.PVar _ | Ast.PConstant _ -> empty
  | Ast.PTuple (p1, p2, rest) ->
    of_list (p1 :: p2 :: rest)
    <+> (shrink_pattern p1 >|= fun p1 -> Ast.PTuple (p1, p2, rest))
    <+> (shrink_pattern p2 >|= fun p2 -> Ast.PTuple (p1, p2, rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_pattern rest >|= fun rest -> Ast.PTuple (p1, p2, rest))
;;

let rec shrink_expr e =
  let open QCheck.Iter in
  match e with
  | Ast.EConst _ | Ast.EIdent _ -> empty
  | Ast.EFun (p, body) ->
    of_list [ body ]
    <+> (shrink_pattern p >|= fun p -> Ast.EFun (p, body))
    <+> (shrink_expr body >|= fun body -> Ast.EFun (p, body))
  | Ast.EApply (f, arg) ->
    of_list [ f; arg ]
    <+> (shrink_expr f >|= fun f -> Ast.EApply (f, arg))
    <+> (shrink_expr arg >|= fun arg -> Ast.EApply (f, arg))
  | Ast.ETuple (e1, e2, rest) ->
    of_list (e1 :: e2 :: rest)
    <+> (shrink_expr e1 >|= fun e1 -> Ast.ETuple (e1, e2, rest))
    <+> (shrink_expr e2 >|= fun e2 -> Ast.ETuple (e1, e2, rest))
    <+> (QCheck.Shrink.list ~shrink:shrink_expr rest >|= fun rest -> Ast.ETuple (e1, e2, rest))
  | Ast.EIf (c, t, None) ->
    of_list [ c; t ]
    <+> (shrink_expr c >|= fun c -> Ast.EIf (c, t, None))
    <+> (shrink_expr t >|= fun t -> Ast.EIf (c, t, None))
  | Ast.EIf (c, t, Some f) ->
    of_list [ c; t; f ]
    <+> (shrink_expr c >|= fun c -> Ast.EIf (c, t, Some f))
    <+> (shrink_expr t >|= fun t -> Ast.EIf (c, t, Some f))
    <+> (shrink_expr f >|= fun f -> Ast.EIf (c, t, Some f))
  | Ast.ELet (rf, vb, extra, body) ->
    of_list [ vb.vb_expr; body ]
    <+> (shrink_pattern vb.vb_pat
         >|= fun vb_pat -> Ast.ELet (rf, { vb with vb_pat }, extra, body))
    <+> (shrink_expr vb.vb_expr
         >|= fun vb_expr -> Ast.ELet (rf, { vb with vb_expr }, extra, body))
    <+> (shrink_expr body >|= fun body -> Ast.ELet (rf, vb, extra, body))
  | Ast.EFunction (c, cs) ->
    of_list (c.case_expr :: List.map (fun (c : Ast.case) -> c.case_expr) cs)
    <+> (shrink_pattern c.case_pat
         >|= fun case_pat -> Ast.EFunction ({ c with case_pat }, cs))
    <+> (shrink_expr c.case_expr
         >|= fun case_expr -> Ast.EFunction ({ c with case_expr }, cs))
  | Ast.EMatch (scrutinee, c, cs) ->
    of_list (scrutinee :: c.case_expr :: List.map (fun (c : Ast.case) -> c.case_expr) cs)
    <+> (shrink_expr scrutinee >|= fun scrutinee -> Ast.EMatch (scrutinee, c, cs))
    <+> (shrink_pattern c.case_pat
         >|= fun case_pat -> Ast.EMatch (scrutinee, { c with case_pat }, cs))
    <+> (shrink_expr c.case_expr
         >|= fun case_expr -> Ast.EMatch (scrutinee, { c with case_expr }, cs))
;;

let shrink_structure_item =
  let open QCheck.Iter in
  function
  | Ast.SEval e -> shrink_expr e >|= fun e -> Ast.SEval e
  | Ast.SValue (rf, vb, extra) ->
    (shrink_pattern vb.vb_pat >|= fun vb_pat -> Ast.SValue (rf, { vb with vb_pat }, extra))
    <+> (shrink_expr vb.vb_expr
         >|= fun vb_expr -> Ast.SValue (rf, { vb with vb_expr }, extra))
;;

let shrink_structure structure = QCheck.Shrink.list ~shrink:shrink_structure_item structure

let arb_structure =
  QCheck.make ~print:Ast.show_structure ~shrink:shrink_structure Ast_gen.gen_structure
;;

let test_roundtrip (p : parser) =
  QCheck.Test.make
    ~name:(p.name ^ " round-trip: parse (print s) = s")
    ~count:300
    arb_structure
    (fun s ->
       match p.parse (Pprint.string_of_structure s) with
       | Ok s' -> s = s'
       | Error _ -> false)
;;

let () =
  let tests = [ test_roundtrip top_down; test_roundtrip bottom_up ] in
  exit (QCheck_runner.run_tests ~verbose:true tests)
;;
