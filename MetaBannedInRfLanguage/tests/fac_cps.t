CPS factorial

  $ cat > fac_cps.ml <<EOF
  > let rec fac_cps n k =
  >   if n <= 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))
  > ;;
  > let main = fac_cps 5 (fun x -> x)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after parsing fac_cps.ml
  [(SValue (Rec,
      { vb_pat = (PVar "fac_cps");
        vb_expr =
        (EFun ((PVar "n"),
           (EFun ((PVar "k"),
              (EIf (
                 (EApply ((EApply ((EIdent "<="), (EIdent "n"))),
                    (EConst (CInt 1)))),
                 (EApply ((EIdent "k"), (EConst (CInt 1)))),
                 (Some (EApply (
                          (EApply ((EIdent "fac_cps"),
                             (EApply ((EApply ((EIdent "-"), (EIdent "n"))),
                                (EConst (CInt 1))))
                             )),
                          (EFun ((PVar "p"),
                             (EApply ((EIdent "k"),
                                (EApply ((EApply ((EIdent "*"), (EIdent "p"))),
                                   (EIdent "n")))
                                ))
                             ))
                          )))
                 ))
              ))
           ))
        },
      []));
    (SValue (NonRec,
       { vb_pat = (PVar "main");
         vb_expr =
         (EApply ((EApply ((EIdent "fac_cps"), (EConst (CInt 5)))),
            (EFun ((PVar "x"), (EIdent "x")))))
         },
       []))
    ]

Both parsers produce the same AST

  $ ../driver/compiler.exe --stop-after parsing -parsing topdown fac_cps.ml > topdown.txt
  $ ../driver/compiler.exe --stop-after parsing -parsing menhir fac_cps.ml > menhir.txt
  $ diff topdown.txt menhir.txt
