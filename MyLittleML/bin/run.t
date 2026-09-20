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

  $ printf 'let x = 5 in x = 5' | ../compiler.exe --parser topdown
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Eq, (Ast.Var "x"), (Ast.Int 5))))))
    ]

  $ printf '1 + 2 * 3' | ../compiler.exe --parser topdown
  [(Ast.Binop (Ast.Add, (Ast.Int 1),
      (Ast.Binop (Ast.Mul, (Ast.Int 2), (Ast.Int 3)))))
    ]

  $ printf 'let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n))' | ../compiler.exe --parser topdown
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

  $ printf '(* c *)\nlet x = 5 in x + 1' | ../compiler.exe --parser topdown
  [(Ast.Let ({ Ast.rec_ = false; name = "x"; params = []; body = (Ast.Int 5) },
      (Some (Ast.Binop (Ast.Add, (Ast.Var "x"), (Ast.Int 1))))))
    ]

  $ printf 'letx = 1' | ../compiler.exe --parser topdown
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
