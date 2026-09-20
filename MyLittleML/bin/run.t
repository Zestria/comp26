$ ../compiler.exe --help

  $ ../compiler.exe --bogus
  compiler: unrecognized option: --bogus
  Try 'compiler.exe --help' for more information.
  [2]

  $ ../compiler.exe --parser foo
  compiler: unknown parser: foo
  Try 'compiler.exe --help' for more information.
  [2]

  $ ../compiler.exe does-not-exist.ml
  compiler: cannot open file: does-not-exist.ml: No such file or directory
  [1]

  $ printf 'let x = 5 in x = 5' | ../compiler.exe --parser bottomup
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Eq, (Ast.Var "x"), (Ast.Int 5))))))
    ]

  $ printf '1 + 2 * 3' | ../compiler.exe --parser bottomup
  [(Ast.Binop (Ast.Add, (Ast.Int 1),
      (Ast.Binop (Ast.Mul, (Ast.Int 2), (Ast.Int 3)))))
    ]

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe --parser bottomup
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
