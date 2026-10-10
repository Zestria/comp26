[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

module Gen = QCheck.Gen

let candidate_names =
  [ "x"; "y"; "z"; "a"; "b"; "c"; "n"; "m"; "f"; "g"; "h"; "acc"; "res" ]
;;

let () = List.iter (fun name -> assert (not (Syntax.is_reserved name))) candidate_names

let gen_ident : Ast.ident Gen.t = Gen.oneof_list candidate_names

let gen_constant : Ast.constant Gen.t =
  Gen.oneof
    [ Gen.map (fun i -> Ast.CInt i) (Gen.int_range 0 20)
    ; Gen.map (fun b -> Ast.CBool b) Gen.bool
    ; Gen.return Ast.CUnit
    ]
;;

let gen_rec_flag : Ast.rec_flag Gen.t = Gen.oneof_list [ Ast.Rec; Ast.NonRec ]

let rec gen_pattern n : Ast.pattern Gen.t =
  if n <= 0
  then
    Gen.oneof
      [ Gen.return Ast.PAny
      ; Gen.map (fun id -> Ast.PVar id) gen_ident
      ; Gen.map (fun c -> Ast.PConstant c) gen_constant
      ]
  else
    Gen.oneof
      [ Gen.return Ast.PAny
      ; Gen.map (fun id -> Ast.PVar id) gen_ident
      ; Gen.map (fun c -> Ast.PConstant c) gen_constant
      ; Gen.map3
          (fun p1 p2 rest -> Ast.PTuple (p1, p2, rest))
          (gen_pattern (n / 2))
          (gen_pattern (n / 2))
          (Gen.list_size (Gen.int_range 0 2) (gen_pattern (n / 2)))
      ]
;;

(* Eta-expanded on purpose: sub-generators of all branches are then built only
   for the branch [oneof] actually picks, not eagerly for the whole tree. *)
let rec gen_expression n : Ast.expression Gen.t = fun st -> gen_expression_branches n st

and gen_expression_branches n : Ast.expression Gen.t =
  if n <= 0
  then
    Gen.oneof
      [ Gen.map (fun c -> Ast.EConst c) gen_constant
      ; Gen.map (fun id -> Ast.EIdent id) gen_ident
      ]
  else (
    let half = gen_expression (n / 2) in
    let third = gen_expression (n / 3) in
    Gen.oneof
      [ Gen.map (fun c -> Ast.EConst c) gen_constant
      ; Gen.map (fun id -> Ast.EIdent id) gen_ident
      ; Gen.map2
          (fun p body -> Ast.EFun (p, body))
          (gen_pattern 2)
          (gen_expression (pred n))
      ; Gen.map2 (fun f arg -> Ast.EApply (f, arg)) half half
      ; Gen.map3
          (fun e1 e2 rest -> Ast.ETuple (e1, e2, rest))
          third
          third
          (Gen.list_size (Gen.int_range 0 2) third)
      ; Gen.map3 (fun c t e -> Ast.EIf (c, t, e)) third third (Gen.option third)
      ; Gen.map2
          (fun vb body -> Ast.ELet (Rec, vb, [], body))
          (gen_value_binding (n / 2))
          half
      ; Gen.map2
          (fun vb body -> Ast.ELet (NonRec, vb, [], body))
          (gen_value_binding (n / 2))
          half
      ; Gen.map2
          (fun c cs -> Ast.EFunction (c, cs))
          (gen_case (n / 2))
          (Gen.list_size (Gen.int_range 0 2) (gen_case (n / 2)))
      ; Gen.map3
          (fun scrutinee c cs -> Ast.EMatch (scrutinee, c, cs))
          third
          (gen_case (n / 3))
          (Gen.list_size (Gen.int_range 0 2) (gen_case (n / 3)))
      ])

and gen_value_binding n : Ast.value_binding Gen.t =
  Gen.map2
    (fun vb_pat vb_expr -> { Ast.vb_pat; vb_expr })
    (gen_pattern 2)
    (gen_expression n)

and gen_case n : Ast.case Gen.t =
  Gen.map2
    (fun case_pat case_expr -> { Ast.case_pat; case_expr })
    (gen_pattern 2)
    (gen_expression n)
;;

let gen_expression_sized : Ast.expression Gen.t =
  Gen.sized_size (Gen.int_bound 20) gen_expression
;;

let gen_structure_item n : Ast.structure_item Gen.t =
  Gen.oneof
    [ Gen.map (fun e -> Ast.SEval e) (gen_expression n)
    ; Gen.map2 (fun rf vb -> Ast.SValue (rf, vb, [])) gen_rec_flag (gen_value_binding n)
    ]
;;

let gen_structure : Ast.structure Gen.t =
  Gen.list_size (Gen.int_range 1 5) (Gen.sized_size (Gen.int_bound 20) gen_structure_item)
;;

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
  QCheck.make ~print:Ast.show_structure ~shrink:shrink_structure gen_structure
;;

let arb_expression =
  QCheck.make ~print:Ast.show_expression ~shrink:shrink_expr gen_expression_sized
;;
