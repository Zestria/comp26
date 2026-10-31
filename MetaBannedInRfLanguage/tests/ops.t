Operator overrides with -ops (top-down parser only)

Associativity: by default '-' is left-associative

  $ cat > assoc.ml <<EOF
  > let x = 1 - 2 - 3
  > EOF

  $ ../driver/compiler.exe --stop-after parsing assoc.ml
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

  $ ../driver/compiler.exe --stop-after parsing -ops "right 3 -" assoc.ml
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

  $ ../driver/compiler.exe --stop-after parsing prec.ml
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

  $ ../driver/compiler.exe --stop-after parsing -ops "left 5 +" prec.ml
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

  $ ../driver/compiler.exe --stop-after parsing dollar.ml
  compiler: dollar.ml: parsing error: end_of_input
  [1]

  $ ../driver/compiler.exe --stop-after parsing -ops "left 5 \$" dollar.ml
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

  $ ../driver/compiler.exe --stop-after parsing prefix.ml
  compiler: prefix.ml: parsing error: end_of_input
  [1]

  $ ../driver/compiler.exe --stop-after parsing -ops "prefix 1 ~" prefix.ml
  [(SValue (NonRec,
      { vb_pat = (PVar "x");
        vb_expr =
        (EApply ((EIdent "~"), (EApply ((EIdent "f"), (EIdent "y"))))) },
      []))
    ]

  $ ../driver/compiler.exe --stop-after parsing -ops "prefix 2 ~" prefix.ml
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

  $ ../driver/compiler.exe --stop-after parsing -ops "right 3 - ; left 5 +" mixed.ml
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

  $ ../driver/compiler.exe --stop-after parsing -parsing menhir -ops "left 5 +" prec.ml
  compiler: -ops is supported only with -parsing topdown
  [1]

Malformed settings

  $ ../driver/compiler.exe --stop-after parsing -ops "left" prec.ml
  compiler: settings group "left" lacks a number
  [1]

  $ ../driver/compiler.exe --stop-after parsing -ops "left 5" prec.ml
  compiler: settings group "left" "5" has no operators
  [1]

  $ ../driver/compiler.exe --stop-after parsing -ops "middle 5 +" prec.ml
  compiler: expected left, right or prefix, got "middle"
  [1]

  $ ../driver/compiler.exe --stop-after parsing -ops "left x +" prec.ml
  compiler: expected a number after "left", got "x"
  [1]

  $ ../driver/compiler.exe --stop-after parsing -ops "left 5 + ; right 3 +" prec.ml
  compiler: operator "+" is overridden twice (in infix settings)
  [1]

A prefix minus in parentheses is not an operator value

  $ cat > paren.ml <<EOF
  > let a = (-1);;
  > let f x = g (-x) (x * (-2));;
  > let m = ( - );;
  > let t = (1, -2)
  > EOF

  $ ../driver/compiler.exe --stop-after parsing -parsing topdown paren.ml > topdown.txt
  $ ../driver/compiler.exe --stop-after parsing -parsing menhir paren.ml > menhir.txt
  $ diff topdown.txt menhir.txt

Binary operator values and unary signs have different arities

  $ cat > signs.ml <<EOF
  > let inc = (+) 1;;
  > let from_fifty = (-) 50;;
  > let negate x = -x;;
  > let identity x = +x
  > EOF
  $ ../driver/compiler.exe --stop-after anf signs.ml
  let inc = ( + ) 1;;
  let from_fifty = ( - ) 50;;
  let negate x = - x;;
  let identity x = + x;;

Values, partial application, and passing operators to functions

  $ cat > operators.ml <<EOF
  > let inc = (+) 1;;
  > let from_fifty = (-) 50;;
  > let negate = (~-);;
  > let positive = (~+);;
  > let apply f x = f x;;
  > let a = inc 41;;
  > let b = from_fifty 8;;
  > let c = apply negate (-42);;
  > let d = apply positive 42;;
  > let e = if apply not false then 42 else 0
  > EOF
  $ ../driver/compiler.exe -parsing topdown --stop-after parsing operators.ml > topdown.txt
  $ ../driver/compiler.exe -parsing menhir --stop-after parsing operators.ml > menhir.txt
  $ diff topdown.txt menhir.txt
  $ ./interp_check.exe operators.ml
  a = 42
  b = 42
  c = 42
  d = 42
  e = 42

Wrappers retain the primitive when its name is rebound later

  $ cat > shadow.ml <<EOF
  > let choose b = if b then not else (fun x -> x);;
  > let not x = x;;
  > let f = choose true;;
  > let result = if f false then 42 else 0
  > EOF
  $ ../driver/compiler.exe -parsing topdown --stop-after parsing shadow.ml > topdown.txt
  $ ../driver/compiler.exe -parsing menhir --stop-after parsing shadow.ml > menhir.txt
  $ diff topdown.txt menhir.txt
  $ ./interp_check.exe shadow.ml
  result = 42

A boolean operator called through a variable evaluates both arguments

  $ cat > eager.ml <<EOF
  > let conjunction = (&&);;
  > let result = conjunction false (1 / 0 = 0)
  > EOF
  $ ./interp_check.exe eager.ml
  interp_check: AST evaluation error: division by zero
  [1]

  $ cat > lazy.ml <<EOF
  > let result = false && (1 / 0 = 0)
  > EOF
  $ ./interp_check.exe lazy.ml
  result = false

The right-hand expression stays inside the conditional branch

  $ cat > short_circuit.ml <<EOF
  > let guarded_and a n = a && (1 / n = 0);;
  > let guarded_or a n = a || (1 / n = 0);;
  > let and_skip = guarded_and false 0;;
  > let or_skip = guarded_or true 0;;
  > let and_eval = guarded_and true 2;;
  > let or_eval = guarded_or false 2;;
  > let const_and = false && (1 / 0 = 0);;
  > let const_or = true || (1 / 0 = 0)
  > EOF
  $ ./interp_check.exe short_circuit.ml
  and_skip = false
  or_skip = true
  and_eval = true
  or_eval = true
  const_and = false
  const_or = true

The right-hand expression is evaluated when needed

  $ cat > and_fail.ml <<EOF
  > let result = true && (1 / 0 = 0)
  > EOF
  $ ./interp_check.exe and_fail.ml
  interp_check: AST evaluation error: division by zero
  [1]

  $ cat > or_fail.ml <<EOF
  > let result = false || (1 / 0 = 0)
  > EOF
  $ ./interp_check.exe or_fail.ml
  interp_check: AST evaluation error: division by zero
  [1]
