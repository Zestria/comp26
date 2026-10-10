$ ../compiler.exe --help

  $ ../compiler.exe --bogus
  compiler: unrecognized option: --bogus
  Try 'compiler.exe --help' for more information.
  [2]

  $ ../compiler.exe --parser foo
  compiler: unknown parser: foo (expected 'topdown' or 'menhir')
  Try 'compiler.exe --help' for more information.
  [2]

  $ ../compiler.exe does-not-exist.ml
  compiler: cannot open file: does-not-exist.ml: No such file or directory
  [1]

  $ printf 'let x = 5 in x = 5' | ../compiler.exe --parser bottomup --stop-after parsing
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Eq, (Ast.Var "x"), (Ast.Int 5))))))
    ]

  $ printf '1 + 2 * 3' | ../compiler.exe --parser bottomup --stop-after parsing
  [(Ast.Binop (Ast.Add, (Ast.Int 1),
      (Ast.Binop (Ast.Mul, (Ast.Int 2), (Ast.Int 3)))))
    ]

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe --parser bottomup --stop-after parsing
  [(Ast.Let (
      { Ast.rec_ = true; name = "fact"; params = ["n"; "k"];
        body =
        (Ast.If ((Ast.Binop (Ast.Le, (Ast.Var "n"), (Ast.Int 1))),
           (Ast.App ((Ast.Var "k"), (Ast.Int 1))),
           (Ast.App (
              (Ast.App ((Ast.Var "fact"),
                 (Ast.Binop (Ast.Sub, (Ast.Var "n"), (Ast.Int 1))))),
              (Ast.Fun (["r"],
                 (Ast.App ((Ast.Var "k"),
                    (Ast.Binop (Ast.Mul, (Ast.Var "r"), (Ast.Var "n")))))
                 ))
              ))
           ))
        },
      None))
    ]

  $ printf 'let x = ' | ../compiler.exe --parser bottomup
  compiler: 1:9: syntax error
  [1]

  $ printf 'let x = 5 in x = 5' | ../compiler.exe --parser topdown --stop-after parsing
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Eq, (Ast.Var "x"), (Ast.Int 5))))))
    ]

  $ printf '1 + 2 * 3' | ../compiler.exe --parser topdown --stop-after parsing
  [(Ast.Binop (Ast.Add, (Ast.Int 1),
      (Ast.Binop (Ast.Mul, (Ast.Int 2), (Ast.Int 3)))))
    ]

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe --parser topdown --stop-after parsing
  [(Ast.Let (
      { Ast.rec_ = true; name = "fact"; params = ["n"; "k"];
        body =
        (Ast.If ((Ast.Binop (Ast.Le, (Ast.Var "n"), (Ast.Int 1))),
           (Ast.App ((Ast.Var "k"), (Ast.Int 1))),
           (Ast.App (
              (Ast.App ((Ast.Var "fact"),
                 (Ast.Binop (Ast.Sub, (Ast.Var "n"), (Ast.Int 1))))),
              (Ast.Fun (["r"],
                 (Ast.App ((Ast.Var "k"),
                    (Ast.Binop (Ast.Mul, (Ast.Var "r"), (Ast.Var "n")))))
                 ))
              ))
           ))
        },
      None))
    ]

  $ printf '(* c *)\nlet x = 5 in x + 1' | ../compiler.exe --parser topdown --stop-after parsing
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Add, (Ast.Var "x"), (Ast.Int 1))))))
    ]

  $ printf 'letx = 1' | ../compiler.exe --parser topdown --stop-after parsing
  [(Ast.Binop (Ast.Eq, (Ast.Var "letx"), (Ast.Int 1)))]

  $ printf 'let x = ' | ../compiler.exe --parser topdown
  compiler: 1:9: syntax error
  [1]

  $ printf 'let x = in 5' | ../compiler.exe --parser topdown
  compiler: 1:11: keyword in cannot be used as an identifier
  [1]

  $ printf '5 in x' | ../compiler.exe --parser topdown
  compiler: 1:5: keyword in cannot be used as an identifier
  [1]

  $ printf '(* unterminated' | ../compiler.exe --parser topdown
  compiler: 1:16: unterminated comment
  [1]

  $ printf 'let x = 1' | ../compiler.exe --parser bottomup --sticky
  compiler: sticky mode is not supported by the bottom-up parser
  [1]

  $ printf 'letx = 1' | ../compiler.exe --parser topdown --sticky --stop-after parsing
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 1) },
      None))
    ]

  $ printf 'letrec f n = n + 1' | ../compiler.exe --parser topdown --sticky --stop-after parsing
  [(Ast.Let (
      { Ast.rec_ = true; name = "f"; params = ["n"];
        body = (Ast.Binop (Ast.Add, (Ast.Var "n"), (Ast.Int 1))) },
      None))
    ]

  $ printf 'funx->x+1' | ../compiler.exe --parser topdown --sticky --stop-after parsing
  [(Ast.Fun (["x"], (Ast.Binop (Ast.Add, (Ast.Var "x"), (Ast.Int 1)))))]

  $ printf 'letter = 1' | ../compiler.exe --parser topdown --sticky --stop-after parsing
  [(Ast.Let (
      { Ast.rec_ = false; name = "ter"; params = []; body = (Ast.Int 1) }, None
      ))
    ]

  $ printf 'letx = 1' | ../compiler.exe --parser topdown --stop-after parsing
  [(Ast.Binop (Ast.Eq, (Ast.Var "letx"), (Ast.Int 1)))]

  $ printf 'letx = 1' | ../compiler.exe --parser bottomup --sticky
  compiler: sticky mode is not supported by the bottom-up parser
  [1]

  $ printf 'let x = 5 in x + 1' | ../compiler.exe --stop-after anf
  [(Anf.AExp
      (Anf.ALet (Anf.Nonrec, "x", (Anf.ACexp (Anf.CAtom (Anf.AInt 5))),
         (Anf.ACexp (Anf.CBinop (Ast.Add, (Anf.AVar "x"), (Anf.AInt 1)))))))
    ]

  $ printf 'let x = 5 in x + 1' | ../compiler.exe --stop-after closure-conversion
  [(Anf.AExp
      (Anf.ALet (Anf.Nonrec, "x", (Anf.ACexp (Anf.CAtom (Anf.AInt 5))),
         (Anf.ACexp (Anf.CBinop (Ast.Add, (Anf.AVar "x"), (Anf.AInt 1)))))))
    ]

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe --stop-after anf | grep -c 'CFun'
  2

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe --stop-after closure-conversion | grep -c 'cc_f'
  2

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe | grep -c 'll_f'
  2

  $ printf 'let x = 5 in x + 1' | ../compiler.exe -parsing menhir --stop-after parsing
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Add, (Ast.Var "x"), (Ast.Int 1))))))
    ]

  $ printf 'let x = 5 in x + 1' | ../compiler.exe -o - --stop-after anf | head -2
  [(Anf.AExp
      (Anf.ALet (Anf.Nonrec, "x", (Anf.ACexp (Anf.CAtom (Anf.AInt 5))),

  $ printf 'let x = 5 in x + 1' | ../compiler.exe -o out.anf --stop-after anf && cat out.anf | head -2
  [(Anf.AExp
      (Anf.ALet (Anf.Nonrec, "x", (Anf.ACexp (Anf.CAtom (Anf.AInt 5))),
  $ rm -f out.anf

  $ ../compiler.exe --stop-after bogus < /dev/null
  compiler: unknown stage: bogus (expected 'parsing', 'anf', 'closure-conversion' or 'lambda-lifting')
  Try 'compiler.exe --help' for more information.
  [2]
