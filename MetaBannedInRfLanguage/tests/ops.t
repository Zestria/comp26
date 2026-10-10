Operator overrides with -ops (top-down parser only)

Associativity: by default '-' is left-associative

  $ cat > assoc.ml <<EOF
  > let x = 1 - 2 - 3
  > EOF

  $ ../driver/compiler.exe assoc.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply (
           (EApply ((EIdent "-"),
              (EApply ((EApply ((EIdent "-"), (EConst (CInt 1)))),
                 (EConst (CInt 2))))
              )),
           (EConst (CInt 3))))
        },
      []))
    ]

  $ ../driver/compiler.exe -ops "right 3 -" assoc.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EApply ((EIdent "-"), (EConst (CInt 1)))),
           (EApply ((EApply ((EIdent "-"), (EConst (CInt 2)))),
              (EConst (CInt 3))))
           ))
        },
      []))
    ]

Precedence: by default '*' binds tighter than '+'

  $ cat > prec.ml <<EOF
  > let x = 1 + 2 * 3
  > EOF

  $ ../driver/compiler.exe prec.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EApply ((EIdent "+"), (EConst (CInt 1)))),
           (EApply ((EApply ((EIdent "*"), (EConst (CInt 2)))),
              (EConst (CInt 3))))
           ))
        },
      []))
    ]

  $ ../driver/compiler.exe -ops "left 5 +" prec.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply (
           (EApply ((EIdent "*"),
              (EApply ((EApply ((EIdent "+"), (EConst (CInt 1)))),
                 (EConst (CInt 2))))
              )),
           (EConst (CInt 3))))
        },
      []))
    ]

A new infix operator is unknown without -ops

  $ cat > dollar.ml <<EOF
  > let x = 1 + 2 $ 3
  > EOF

  $ ../driver/compiler.exe dollar.ml
  compiler: dollar.ml: parsing error: end_of_input
  [1]

  $ ../driver/compiler.exe -ops "left 5 \$" dollar.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EApply ((EIdent "+"), (EConst (CInt 1)))),
           (EApply ((EApply ((EIdent "$"), (EConst (CInt 2)))),
              (EConst (CInt 3))))
           ))
        },
      []))
    ]

Prefix operator arity

  $ cat > prefix.ml <<EOF
  > let x = ~ f y
  > EOF

  $ ../driver/compiler.exe prefix.ml
  compiler: prefix.ml: parsing error: end_of_input
  [1]

  $ ../driver/compiler.exe -ops "prefix 1 ~" prefix.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EIdent "~"), (EApply ((EIdent "f"), (EIdent "y"))))) },
      []))
    ]

  $ ../driver/compiler.exe -ops "prefix 2 ~" prefix.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EApply ((EIdent "~"), (EIdent "f"))), (EIdent "y"))) },
      []))
    ]

Several groups at once

  $ cat > mixed.ml <<EOF
  > let x = 1 - 2 - 3 * 4 + 5
  > EOF

  $ ../driver/compiler.exe -ops "right 3 - ; left 5 +" mixed.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EApply ((EIdent "-"), (EConst (CInt 1)))),
           (EApply ((EApply ((EIdent "-"), (EConst (CInt 2)))),
              (EApply ((EApply ((EIdent "*"), (EConst (CInt 3)))),
                 (EApply ((EApply ((EIdent "+"), (EConst (CInt 4)))),
                    (EConst (CInt 5))))
                 ))
              ))
           ))
        },
      []))
    ]

The menhir parser has a fixed grammar, so -ops is rejected

  $ ../driver/compiler.exe -parsing menhir -ops "left 5 +" prec.ml
  compiler: -ops is supported only with -parsing topdown
  [1]

Malformed settings

  $ ../driver/compiler.exe -ops "left" prec.ml
  compiler: settings group "left" lacks a number
  [1]

  $ ../driver/compiler.exe -ops "left 5" prec.ml
  compiler: settings group "left" "5" has no operators
  [1]

  $ ../driver/compiler.exe -ops "middle 5 +" prec.ml
  compiler: expected left, right or prefix, got "middle"
  [1]

  $ ../driver/compiler.exe -ops "left x +" prec.ml
  compiler: expected a number after "left", got "x"
  [1]

  $ ../driver/compiler.exe -ops "left 5 + ; right 3 +" prec.ml
  compiler: operator "+" is overridden twice (in infix settings)
  [1]

A prefix minus in parentheses is not an operator value

  $ cat > paren.ml <<EOF
  > let a = (-1);;
  > let f x = g (-x) (x * (-2));;
  > let m = ( - );;
  > let t = (1, -2)
  > EOF

  $ ../driver/compiler.exe -parsing topdown paren.ml > topdown.txt
  $ ../driver/compiler.exe -parsing menhir paren.ml > menhir.txt
  $ diff topdown.txt menhir.txt
