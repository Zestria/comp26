CPS factorial and fibonacci, as required by task 2.

  $ cat > fac.ml <<'EOF'
  > let rec fac n k =
  >   if n <= 1 then k 1
  >   else fac (n - 1) (fun x -> k (n * x))
  > in fac 5 (fun x -> x)
  > EOF

  $ cat > fib.ml <<'EOF'
  > let rec fib n k =
  >   if n <= 1 then k n
  >   else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
  > in fib 10 (fun x -> x)
  > EOF

  $ ../compiler.exe --stop-after anf fac.ml
  let rec fac n k =
    if n <= 1
    then k 1
    else
      let t0 = n - 1 in
      let t2 =
        (fun x ->
           let t1 = n * x in
           k t1)
      in
      fac t0 t2
  in
  let t3 = (fun x_1 -> x_1) in
  fac 5 t3

  $ ../compiler.exe --stop-after anf fib.ml
  let rec fib n k =
    if n <= 1
    then k n
    else
      let t0 = n - 1 in
      let t4 =
        (fun a ->
           let t1 = n - 2 in
           let t3 =
             (fun b ->
                let t2 = a + b in
                k t2)
           in
           fib t1 t3)
      in
      fib t0 t4
  in
  let t5 = (fun x -> x) in
  fib 10 t5

The CPS tests of the course are programs of declarations
(manytests/typed/012faccps.ml, 012fibcps.ml, 010faccps_ll.ml, 010fibcps_ll.ml).
Every declaration is converted on its own, declared names are kept:

  $ cat > fac_decls.ml <<'EOF'
  > let rec fac n k =
  >   if n < 2
  >   then k 1
  >   else fac (n - 1) (fun a -> k (a * n))
  > let main = print_int (fac 6 (fun x -> x))
  > EOF
  $ ../compiler.exe fac_decls.ml
  let rec fac n k =
    if n < 2
    then k 1
    else
      let t0 = n - 1 in
      let t2 =
        (fun a ->
           let t1 = a * n in
           k t1)
      in
      fac t0 t2
  
  let main =
    let t0 = (fun x -> x) in
    let t1 = fac 6 t0 in
    print_int t1

  $ cat > fib_decls.ml <<'EOF'
  > let rec fib n k =
  >   if n < 2
  >   then k n
  >   else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
  > let main = print_int (fib 6 (fun x -> x))
  > EOF
  $ ../compiler.exe fib_decls.ml
  let rec fib n k =
    if n < 2
    then k n
    else
      let t0 = n - 1 in
      let t4 =
        (fun a ->
           let t1 = n - 2 in
           let t3 =
             (fun b ->
                let t2 = a + b in
                k t2)
           in
           fib t1 t3)
      in
      fib t0 t4
  
  let main =
    let t0 = (fun x -> x) in
    let t1 = fib 6 t0 in
    print_int t1

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
  $ ../compiler.exe fac_ll.ml
  let id x = x
  
  let fresh_1 n k p =
    let t0 = p * n in
    k t0
  
  let rec fac_cps n k =
    if n = 1
    then k 1
    else
      let t0 = n - 1 in
      let t1 = fresh_1 n k in
      fac_cps t0 t1
  
  let main =
    let t0 = fac_cps 4 id in
    let _ = print_int t0 in
    0

  $ cat > fib_ll.ml <<'EOF'
  > let id x = x
  > let fresh_2 p1 k p2 =
  >   k (p1 + p2)
  > let fresh_1 n k fib p1 =
  >   fib (n-2) (fresh_2 p1 k)
  > let rec fib n k =
  >   if n < 2
  >   then k n
  >   else fib (n - 1) (fresh_1 n k fib)
  > let main =
  >   let z = print_int (fib 6 id)  in
  >   0
  > EOF
  $ ../compiler.exe fib_ll.ml
  let id x = x
  
  let fresh_2 p1 k p2 =
    let t0 = p1 + p2 in
    k t0
  
  let fresh_1 n k fib p1 =
    let t0 = n - 2 in
    let t1 = fresh_2 p1 k in
    fib t0 t1
  
  let rec fib n k =
    if n < 2
    then k n
    else
      let t0 = n - 1 in
      let t1 = fresh_1 n k fib in
      fib t0 t1
  
  let main =
    let t0 = fib 6 id in
    let z = print_int t0 in
    0

Declared names are never renamed or dropped, even when they are shadowed or
unused; [let rec f x = f x] stays recursive:

  $ printf 'let x = 1\nlet x = x + 1\nlet f y = g y\nlet rec h y = h y\nlet _ = 5\n' | ../compiler.exe
  let x = 1
  
  let x = x + 1
  
  let f = g
  
  let rec h y = h y
  
  let _ = 5

Both parsers give the same ANF, and ANF is the default last stage:

  $ ../compiler.exe -parsing menhir fac.ml > fac_menhir.anf
  $ ../compiler.exe -parsing topdown fac.ml > fac_topdown.anf
  $ diff fac_menhir.anf fac_topdown.anf
  $ ../compiler.exe -parsing menhir fib.ml > fib_menhir.anf
  $ ../compiler.exe -parsing topdown fib.ml | diff fib_menhir.anf -

The output is a valid program, and it is already in ANF: converting it again
changes nothing.

  $ ../compiler.exe -o fac.anf fac.ml
  $ ../compiler.exe -o - fac.anf | diff fac.anf -
  $ ../compiler.exe fib.ml > fib.anf
  $ ../compiler.exe -parsing menhir fib.anf | diff fib.anf -
  $ for f in fac_decls fib_decls fac_ll fib_ll; do ../compiler.exe $f.ml > $f.anf; ../compiler.exe $f.anf | diff $f.anf -; done

Keeping ANF small: immediates are not bound, tail calls are not bound,
[f a b] is one call, [let x = <immediate>] is substituted, constant arithmetic
is folded, [fun x -> f x] becomes [f], [- - e] becomes [e] and unused pure
bindings are dropped.

  $ echo "f (g x) (h y)" | ../compiler.exe
  let t0 = g x in
  let t1 = h y in
  f t0 t1
  $ echo "let x = 1 + 2 * 3 in let y = x in f y y" | ../compiler.exe
  f 7 7
  $ echo "let unused = n * 2 in let call = g 1 in 7" | ../compiler.exe
  let call = g 1 in
  7
  $ echo "1 + (let x = 2 in x * y)" | ../compiler.exe
  let t0 = 2 * y in
  1 + t0
  $ echo "let x = f 1 in x" | ../compiler.exe
  f 1
  $ echo "let g = fun x -> h x in g 1" | ../compiler.exe
  h 1
  $ echo "f (fun x y -> k x (let unused = x * y in y))" | ../compiler.exe
  f k
  $ echo "fun x -> x x" | ../compiler.exe
  fun x -> x x
  $ echo "- - x + 1" | ../compiler.exe
  x + 1

Comparisons are tested directly, as RISC-V and LLVM compare and branch in one
instruction. A comparison of constants keeps only one branch.

  $ echo "if f x < g y then 1 else 2" | ../compiler.exe
  let t0 = f x in
  let t1 = g y in
  if t0 < t1
  then 1
  else 2
  $ echo "let c = x < y in if c then f 1 else g 2" | ../compiler.exe
  if x < y
  then f 1
  else g 2
  $ echo "1 + (let c = x = y in if c then f 1 else 2)" | ../compiler.exe
  let t0 =
    (if x = y
     then f 1
     else 2)
  in
  1 + t0
  $ echo "let c = x < y in let a = (if c then 1 else 2) in if c then a else 0" | ../compiler.exe
  let c = x < y in
  let a =
    (if c
     then 1
     else 2)
  in
  if c
  then a
  else 0
  $ echo "if f x then 1 else 2" | ../compiler.exe
  let t0 = f x in
  if t0
  then 1
  else 2
  $ echo "let n = 5 in if n <= 1 then 1 else f n" | ../compiler.exe
  f 5

Shadowed names are renamed, generated names do not clash with user names:

  $ echo "let t0 = f 1 in let x = g t0 in let x = h x in k (p x) (q t0)" | ../compiler.exe
  let t0 = f 1 in
  let x = g t0 in
  let x_1 = h x in
  let t1 = p x_1 in
  let t2 = q t0 in
  k t1 t2

Command line errors:

  $ ../compiler.exe -parsing lr fac.ml
  Error: -parsing expects 'topdown' or 'menhir'
  Try '--help' for usage.
  [2]
  $ ../compiler.exe --stop-after codegen fac.ml
  Error: --stop-after expects 'parsing', 'anf' or 'closure'
  Try '--help' for usage.
  [2]
  $ ../compiler.exe --unknown fac.ml
  Error: unknown option '--unknown'
  Try '--help' for usage.
  [2]
  $ ../compiler.exe -parsing menhir --extended fac.ml
  Error: --extended is only supported by the top-down parser
  Try '--help' for usage.
  [2]
  $ ../compiler.exe missing.ml
  Error: missing.ml: No such file or directory
  [1]
