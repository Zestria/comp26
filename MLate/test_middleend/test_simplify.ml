[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open MLate_frontend.Ast
open MLate_middleend.Anf_ast
module Simplify = MLate_middleend.Simplify

let check before after =
  let program body = [ { flag = Nonrec; name = "result"; body } ] in
  let actual = Simplify.run (program before) in
  assert (actual = program after);
  assert (Simplify.run actual = actual)
;;

let () =
  check
    (ALet
       ( Nonrec
       , "x"
       , CImm (IVar "y")
       , ALet (Nonrec, "z", CImm (IVar "x"), ACExpr (CApp (IVar "f", IVar "z"))) ))
    (ACExpr (CApp (IVar "f", IVar "y")));
  check
    (ALet (Nonrec, "x", CBinop (Plus, IInt 2, IInt 3), ACExpr (CApp (IVar "f", IVar "x"))))
    (ACExpr (CApp (IVar "f", IInt 5)));
  List.iter
    (fun (op, left, right, result) ->
       check (ACExpr (CBinop (op, IInt left, IInt right))) (ACExpr (CImm (IInt result))))
    [ Minus, 2, 3, -1; Times, 3, 4, 12; Lt, 3, 2, 0; Plus, max_int, 1, min_int ];
  check
    (ALet
       ( Nonrec
       , "condition"
       , CBinop (Lt, IInt 2, IInt 3)
       , ALet
           ( Nonrec
           , "r"
           , CIf
               ( IVar "condition"
               , ALet
                   ( Nonrec
                   , "t"
                   , CApp (IVar "g", IInt 1)
                   , ACExpr (CBinop (Plus, IVar "t", IInt 2)) )
               , ACExpr (CApp (IVar "bad", IInt 0)) )
           , ACExpr (CApp (IVar "f", IVar "r")) ) ))
    (ALet
       ( Nonrec
       , "t"
       , CApp (IVar "g", IInt 1)
       , ALet
           ( Nonrec
           , "r"
           , CBinop (Plus, IVar "t", IInt 2)
           , ACExpr (CApp (IVar "f", IVar "r")) ) ));
  check
    (ACExpr (CIf (IInt 0, ACExpr (CApp (IVar "bad", IInt 0)), ACExpr (CImm (IInt 7)))))
    (ACExpr (CImm (IInt 7)));
  check
    (ALet
       ( Nonrec
       , "x"
       , CImm (IInt 5)
       , ACExpr (CFun ("y", ACExpr (CBinop (Plus, IVar "x", IVar "y")))) ))
    (ACExpr (CFun ("y", ACExpr (CBinop (Plus, IInt 5, IVar "y")))));
  List.iter
    (fun value ->
       check
         (ALet (Nonrec, "unused", value, ACExpr (CImm (IInt 0))))
         (ACExpr (CImm (IInt 0))))
    [ CBinop (Plus, IVar "a", IInt 1)
    ; CFun ("parameter", ACExpr (CApp (IVar "print_int", IInt 1)))
    ; CIf (IVar "c", ACExpr (CImm (IInt 1)), ACExpr (CImm (IInt 2)))
    ];
  List.iter
    (fun value ->
       let body = ALet (Nonrec, "unused", value, ACExpr (CImm (IInt 0))) in
       check body body)
    [ CApp (IVar "print_int", IInt 1)
    ; CApp (IVar "loop", IInt 0)
    ; CIf (IVar "c", ACExpr (CApp (IVar "f", IInt 1)), ACExpr (CImm (IInt 2)))
    ];
  let recursive_value = CFun ("x", ACExpr (CApp (IVar "loop", IVar "x"))) in
  check
    (ALet (Rec, "loop", recursive_value, ACExpr (CImm (IInt 0))))
    (ACExpr (CImm (IInt 0)));
  let recursive_body =
    ALet (Rec, "loop", recursive_value, ACExpr (CApp (IVar "loop", IInt 1)))
  in
  check recursive_body recursive_body;
  let first = { flag = Nonrec; name = "a"; body = ACExpr (CImm (IInt 5)) } in
  let second =
    { flag = Nonrec; name = "b"; body = ACExpr (CBinop (Plus, IVar "a", IInt 2)) }
  in
  assert (
    Simplify.run [ first; second ]
    = [ first; { second with body = ACExpr (CImm (IInt 7)) } ])
;;
