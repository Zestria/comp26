[@@@ocaml.text "/*"]

(** Copyright 2021-2026, Kakadu and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open MLate_frontend.Ast
open MLate_middleend.Anf_ast

let check source predicate =
  match MLate_frontend.Parse.parse source with
  | Error error ->
    Format.eprintf "%a@." MLate_frontend.Parse.pp_error error;
    exit 1
  | Ok program ->
    if not (predicate (MLate_middleend.Anf.run program))
    then (
      prerr_endline ("Unexpected ANF: " ^ source);
      exit 1)
;;

let () =
  check "let result = f (g 1) (h 2)" (function
    | Ok
        [ { body =
              ALet
                ( Nonrec
                , a
                , CApp (IVar "g", IInt 1)
                , ALet
                    ( Nonrec
                    , b
                    , CApp (IVar "f", IVar a')
                    , ALet
                        ( Nonrec
                        , c
                        , CApp (IVar "h", IInt 2)
                        , ACExpr (CApp (IVar b', IVar c')) ) ) )
          ; _
          }
        ] -> a = a' && b = b' && c = c'
    | _ -> false);
  check "let x = 10 let result = (let x = 1 in x + 2) + x" (function
    | Ok
        [ { name = outer; _ }
        ; { body =
              ALet
                ( Nonrec
                , inner
                , CImm (IInt 1)
                , ALet
                    ( Nonrec
                    , t
                    , CBinop (Plus, IVar inner', IInt 2)
                    , ACExpr (CBinop (Plus, IVar t', IVar outer')) ) )
          ; _
          }
        ] -> inner = inner' && outer = outer' && t = t' && inner <> outer
    | _ -> false);
  check "let result = (if c then f a else g b) + 1" (function
    | Ok
        [ { body =
              ALet
                ( Nonrec
                , t
                , CIf
                    ( IVar "c"
                    , ACExpr (CApp (IVar "f", IVar "a"))
                    , ACExpr (CApp (IVar "g", IVar "b")) )
                , ACExpr (CBinop (Plus, IVar t', IInt 1)) )
          ; _
          }
        ] -> t = t'
    | _ -> false);
  check "let rec f x = f x" (function
    | Ok
        [ { flag = Rec
          ; name = f
          ; body = ACExpr (CFun (x, ACExpr (CApp (IVar f', IVar x'))))
          }
        ] -> f = f' && x = x'
    | _ -> false);
  check "let result = f (anf0 + 1)" (function
    | Ok [ { name; body = ALet (Nonrec, t, CBinop (Plus, IVar "anf0", IInt 1), _); _ } ]
      -> name <> "anf0" && t <> "anf0"
    | _ -> false);
  check "let result = let x = f 1 in x" (function
    | Ok [ { body = ACExpr (CApp (IVar "f", IInt 1)); _ } ] -> true
    | _ -> false);
  List.iter
    (fun source -> check source Result.is_ok)
    [ "let rec fact n k = if n < 1 then k 1 else fact (n-1) (fun r -> k (n*r)) let \
       result = fact 5 (fun x -> x)"
    ; "let rec fib n k = if n < 2 then k n else fib (n-1) (fun a -> fib (n-2) (fun b -> \
       k (a+b))) let result = fib 8 (fun x -> x)"
    ; "let result = let rec f x = if x < 1 then x else f (x-1) in f 3"
    ];
;;
