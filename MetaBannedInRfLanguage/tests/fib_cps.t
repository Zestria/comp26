CPS fibonacci

  $ cat > fib_cps.ml <<EOF
  > let rec fib_cps n k =
  >   match n with
  >   | 0 -> k 0
  >   | 1 -> k 1
  >   | n -> fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
  > ;;
  > let main = fib_cps 10 (fun x -> x)
  > EOF

  $ ../compiler.exe -parsing topdown --stop-after parsing fib_cps.ml
  [(SValue (Rec,
      { vb_pat = (PVar "fib_cps");
        vb_expr =
        (EFun ((PVar "n"),
           (EFun ((PVar "k"),
              (EMatch ((EIdent "n"),
                 { case_pat = (PConstant (CInt 0));
                   case_expr = (EApply ((EIdent "k"), (EConst (CInt 0)))) },
                 [{ case_pat = (PConstant (CInt 1));
                    case_expr = (EApply ((EIdent "k"), (EConst (CInt 1)))) };
                   { case_pat = (PVar "n");
                     case_expr =
                     (EApply (
                        (EApply ((EIdent "fib_cps"),
                           (EApply ((EApply ((EIdent "-"), (EIdent "n"))),
                              (EConst (CInt 1))))
                           )),
                        (EFun ((PVar "a"),
                           (EApply (
                              (EApply ((EIdent "fib_cps"),
                                 (EApply (
                                    (EApply ((EIdent "-"), (EIdent "n"))),
                                    (EConst (CInt 2))))
                                 )),
                              (EFun ((PVar "b"),
                                 (EApply ((EIdent "k"),
                                    (EApply (
                                       (EApply ((EIdent "+"), (EIdent "a"))),
                                       (EIdent "b")))
                                    ))
                                 ))
                              ))
                           ))
                        ))
                     }
                   ]
                 ))
              ))
           ))
        },
      []));
    (SValue (NonRec,
       { vb_pat = (PVar "main");
         vb_expr =
         (EApply ((EApply ((EIdent "fib_cps"), (EConst (CInt 10)))),
            (EFun ((PVar "x"), (EIdent "x")))))
         },
       []))
    ]

Both parsers produce the same AST

  $ ../compiler.exe -parsing topdown fib_cps.ml > topdown.txt
  $ ../compiler.exe -parsing menhir fib_cps.ml > menhir.txt
  $ diff topdown.txt menhir.txt
