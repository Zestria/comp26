A program is a sequence of declarations, like the CPS tests of the course
(manytests/typed/012faccps.ml and 010faccps_ll.ml):

  $ cat > fac.ml <<'EOF'
  > let rec fac n k =
  >   if n < 2
  >   then k 1
  >   else fac (n - 1) (fun a -> k (a * n))
  > let main = print_int (fac 6 (fun x -> x))
  > EOF
  $ ../compiler.exe --stop-after parsing fac.ml
  let rec fac n = (fun k -> (if (n < 2) then (k 1) else ((fac (n - 1)) (fun a -> (k (a * n))))))
  let main = (print_int ((fac 6) (fun x -> x)))
  $ ../compiler.exe --stop-after parsing --ast fac.ml
  [(DLetRec ("fac", "n",
      (Abs ("k",
         (If ((BinOp (Lt, (Var "n"), (Const 2))), (App ((Var "k"), (Const 1))),
            (App ((App ((Var "fac"), (BinOp (Sub, (Var "n"), (Const 1))))),
               (Abs ("a",
                  (App ((Var "k"), (BinOp (Mul, (Var "a"), (Var "n")))))))
               ))
            ))
         ))
      ));
    (DLet ("main",
       (App ((Var "print_int"),
          (App ((App ((Var "fac"), (Const 6))), (Abs ("x", (Var "x")))))))
       ))
    ]
  let rec fac n = (fun k -> (if (n < 2) then (k 1) else ((fac (n - 1)) (fun a -> (k (a * n))))))
  let main = (print_int ((fac 6) (fun x -> x)))

  $ cat > fac_ll.ml <<'EOF'
  > let id x = x
  > let fresh_1 n k p = k (p * n)
  > let rec fac_cps n k =
  >   if n = 1
  >   then k 1
  >   else fac_cps (n-1) (fresh_1 n k)
  > let main =
  >   let _ = print_int (fac_cps 4 id) in
  >   0
  > EOF
  $ ../compiler.exe --stop-after parsing fac_ll.ml
  let id = (fun x -> x)
  let fresh_1 = (fun n -> (fun k -> (fun p -> (k (p * n)))))
  let rec fac_cps n = (fun k -> (if (n = 1) then (k 1) else ((fac_cps (n - 1)) ((fresh_1 n) k))))
  let main = (let _ = (print_int ((fac_cps 4) id)) in 0)

A program may start with an expression. After a declaration [let ... in] is
an error, as in OCaml:

  $ printf 'f 1\nlet x = 2\n' | ../compiler.exe --stop-after parsing
  (f 1)
  let x = 2
  $ printf 'let x = 1 in x\nlet y = 2\n' | ../compiler.exe --stop-after parsing
  (let x = 1 in x)
  let y = 2
  $ printf 'let x = 1\nlet y = 2 in y\n' | ../compiler.exe --stop-after parsing
  Parser error: syntax error at line 2, column 11
  [1]
  $ printf '' | ../compiler.exe --stop-after parsing
  

Errors point to the line and the column where parsing stopped:

  $ printf 'let x = 1\n\nlet y = (2 +)\n' | ../compiler.exe --stop-after parsing
  Parser error: syntax error at line 3, column 1
  [1]
  $ printf '\n\n  1 +' | ../compiler.exe --stop-after parsing
  Parser error: syntax error at line 3, column 5
  [1]

[-] after a function is subtraction, as in OCaml; a negative argument needs
parentheses:

  $ echo "f (n-1)" | ../compiler.exe --stop-after parsing
  (f (n - 1))
  $ echo "f -1" | ../compiler.exe --stop-after parsing
  (f - 1)
  $ echo "f (-1)" | ../compiler.exe --stop-after parsing
  (f (-1))
  $ echo "f - -1" | ../compiler.exe --stop-after parsing
  (f - (-1))
  $ echo "- f x" | ../compiler.exe --stop-after parsing
  (-(f x))

Arguments are simple, [if], [let] and [fun] need parentheses:

  $ echo "f if a then b else c" | ../compiler.exe --stop-after parsing
  Parser error: syntax error at line 1, column 3
  [1]
  $ echo "f (if a then b else c)" | ../compiler.exe --stop-after parsing
  (f (if a then b else c))
  $ echo "if a then b else c d" | ../compiler.exe --stop-after parsing
  (if a then b else (c d))

[_] can be bound, but it is not a variable:

  $ echo "let _ = f 1 in 0" | ../compiler.exe --stop-after parsing
  (let _ = (f 1) in 0)
  $ echo "fun _ _ -> 1" | ../compiler.exe --stop-after parsing
  (fun _ -> (fun _ -> 1))
  $ echo "let _x = 1 in _x" | ../compiler.exe --stop-after parsing
  (let _x = 1 in _x)
  $ echo "f _" | ../compiler.exe --stop-after parsing
  Parser error: syntax error at line 1, column 3
  [1]

Nested functions, [=] without spaces, [let rec f = fun ...]:

  $ echo "fun x -> fun y -> x" | ../compiler.exe --stop-after parsing
  (fun x -> (fun y -> x))
  $ echo "let x =1 in x" | ../compiler.exe --stop-after parsing
  (let x = 1 in x)
  $ echo "let rec f = fun n -> f n in f" | ../compiler.exe --stop-after parsing
  (let rec f n = (f n) in f)
  $ echo "let rec x = 5 in x" | ../compiler.exe --stop-after parsing
  Parser error: syntax error at line 1, column 1
  [1]

Keywords as identifiers work in declarations too:

  $ printf 'let in = 1\nlet then = in + 1\n' | ../compiler.exe --extended --stop-after parsing
  let in = 1
  let then = (in + 1)
