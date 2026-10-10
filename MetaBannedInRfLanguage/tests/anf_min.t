Nested applications get one temporary per call

  $ cat > app.ml <<EOF
  > let f a b c = a + b * c;;
  > let r = f (f 1 2 3) 4 (f 5 6 7)
  > EOF

  $ ../driver/compiler.exe --stop-after anf app.ml
  let f a b c =
    let t1 = b * c in
    a + t1
  ;;
  let r =
    let t2 = f 1 2 3 in
    let t3 = f 5 6 7 in
    f t2 4 t3
  ;;

Curried lambdas become one function with several parameters

  $ cat > lambda.ml <<EOF
  > let add = fun x -> fun y -> fun z -> x + y + z
  > EOF

  $ ../driver/compiler.exe --stop-after anf lambda.ml
  let add x y z =
    let t1 = x + y in
    t1 + z
  ;;

Nested lets are flattened

  $ cat > lets.ml <<EOF
  > let f n = let a = (let b = n + 1 in b * 2) in a - 3
  > EOF

  $ ../driver/compiler.exe --stop-after anf lets.ml
  let f n =
    let b = n + 1 in
    let a = b * 2 in
    a - 3
  ;;

Aliases and constants are substituted, constant arithmetic is folded

  $ cat > consts.ml <<EOF
  > let f n = let x = 5 in let y = x in let m = n in m + y * 2
  > EOF

  $ ../driver/compiler.exe --stop-after anf consts.ml
  let f n = n + 10;;

Unused pure bindings are removed, calls are kept

  $ cat > dead.ml <<EOF
  > let g x = x;;
  > let f n = let unused = n + 1 in let call = g n in let used = n * 2 in used
  > EOF

  $ ../driver/compiler.exe --stop-after anf dead.ml
  let g x = x;;
  let f n =
    let call = g n in
    n * 2
  ;;

Matching a tuple literal does not build the tuple, unused components are dropped

  $ cat > tuples.ml <<EOF
  > let f a b = match (a, b) with
  >   | (0, y) -> y
  >   | (x, _) -> x;;
  > let snd p = let (_, b) = p in b
  > EOF

  $ ../driver/compiler.exe --stop-after anf tuples.ml
  let f a b =
    let t1 = a = 0 in
    if t1
    then b
    else a
  ;;
  let snd p =
    let (_, b) = p in
    b
  ;;

Tuple parameters are destructured once

  $ cat > params.ml <<EOF
  > let f (a, b) (c, d) = a + d
  > EOF

  $ ../driver/compiler.exe --stop-after anf params.ml
  let f t1 t2 =
    let (a, _) = t1 in
    let (_, d) = t2 in
    a + d
  ;;

A shadowed name is renamed only on conflict

  $ cat > shadow.ml <<EOF
  > let f x = let x = x + 1 in let y = x * 2 in let x = y - x in x + y
  > EOF

  $ ../driver/compiler.exe --stop-after anf shadow.ml
  let f x =
    let x_1 = x + 1 in
    let y = x_1 * 2 in
    let x_2 = y - x_1 in
    x_2 + y
  ;;

Local and mutual recursion

  $ cat > rec.ml <<EOF
  > let f n = let rec go i = if i = 0 then n else go (i - 1) in go 3;;
  > let rec even n = if n = 0 then true else odd (n - 1)
  > and odd n = if n = 0 then false else even (n - 1)
  > EOF

  $ ../driver/compiler.exe --stop-after anf rec.ml
  let f n =
    let rec go i =
      let t1 = i = 0 in
      if t1
      then n
      else
        let t2 = i - 1 in
        go t2
    in
    go 3
  ;;
  let rec even n =
    let t3 = n = 0 in
    if t3
    then true
    else
      let t4 = n - 1 in
      odd t4
  and odd n =
    let t5 = n = 0 in
    if t5
    then false
    else
      let t6 = n - 1 in
      even t6
  ;;

let rec of a non-function is rejected

  $ cat > bad_rec.ml <<EOF
  > let rec x = 1
  > EOF

  $ ../driver/compiler.exe --stop-after anf bad_rec.ml
  compiler: the right-hand side of let rec must be a function
  [1]
