After lambda lifting every closed function is a top-level item

  $ cat > fac_cps.ml <<EOF
  > let rec fac_cps n k =
  >   if n <= 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))
  > ;;
  > let main = fac_cps 5 (fun x -> x)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after ll fac_cps.ml
  let t3__cc1 k n p =
    let t4 = p * n in
    k t4
  ;;
  let rec fac_cps n k =
    let t1 = n <= 1 in
    if t1
    then k 1
    else
      let t2 = n - 1 in
      let t3 = t3__cc1 k n in
      fac_cps t2 t3
  ;;
  let t5 x = x;;
  let main = fac_cps 5 t5;;

  $ cat > fib_cps.ml <<EOF
  > let rec fib_cps n k =
  >   match n with
  >   | 0 -> k 0
  >   | 1 -> k 1
  >   | n -> fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
  > ;;
  > let main = fib_cps 10 (fun x -> x)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after ll fib_cps.ml
  let t6__cc1 a k b =
    let t7 = a + b in
    k t7
  ;;
  let t4__cc2 fib_cps k n a =
    let t5 = n - 2 in
    let t6 = t6__cc1 a k in
    fib_cps t5 t6
  ;;
  let rec fib_cps n k =
    let t1 = n = 0 in
    if t1
    then k 0
    else
      let t2 = n = 1 in
      if t2
      then k 1
      else
        let t3 = n - 1 in
        let t4 = t4__cc2 fib_cps k n in
        fib_cps t3 t4
  ;;
  let t8 x = x;;
  let main = fib_cps 10 t8;;

Lambdas in both branches of an if: LL lifts each one separately

  $ cat > ifthen.ml <<EOF
  > let flag = 1
  > let main = if flag then (fun x -> x + 1) else (fun x -> x * 2)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after ll ifthen.ml
  let flag = 1;;
  let f__ll1 x = x + 1;;
  let f__ll2 x = x * 2;;
  let main =
    if flag
    then f__ll1
    else f__ll2
  ;;

Partial application of a top-level function from several places:
the binder is used more than once, so can_reuse does not fire

  $ cat > partial.ml <<EOF
  > let add x y = x + y
  > let inc = add 1
  > let add2 = add 2
  > let main = inc (add2 5)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after ll partial.ml
  let add x y = x + y;;
  let inc = add 1;;
  let add2 = add 2;;
  let main =
    let t1 = add2 5 in
    inc t1
  ;;

Mutual recursion: LL keeps even/odd in one let rec group

  $ cat > mutual2.ml <<EOF
  > let rec even n = if n = 0 then 1 else odd (n - 1)
  > and odd n = if n = 0 then 0 else even (n - 1)
  > ;;
  > let main = even 10
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after ll mutual2.ml
  let rec even n =
    let t1 = n = 0 in
    if t1
    then 1
    else
      let t2 = n - 1 in
      odd t2
  and odd n =
    let t3 = n = 0 in
    if t3
    then 0
    else
      let t4 = n - 1 in
      even t4
  ;;
  let main = even 10;;

Nested lambdas: LL lifts the inner one first, then the outer

  $ cat > nested_ll.ml <<EOF
  > let main =
  >   (fun x ->
  >     let g = fun y -> x + y in
  >     g 1)
  >   10
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after ll nested_ll.ml
  let g y = 10 + y;;
  let main = g 1;;

Recursive calls pass captures explicitly before lambda lifting

  $ cat > capture.ml <<EOF
  > let make n =
  >   let rec loop x = if x = 0 then n else loop (x - 1) in
  >   loop
  > ;;
  > let result = make 42 3
  > EOF
  $ ../driver/compiler.exe --stop-after cc capture.ml
  let make n =
    let rec loop n x =
      let t1 = x = 0 in
      if t1
      then n
      else
        let t2 = x - 1 in
        loop n t2
    in
    loop n
  ;;
  let result = make 42 3;;

  $ ../driver/compiler.exe --stop-after ll capture.ml
  let rec loop n x =
    let t1 = x = 0 in
    if t1
    then n
    else
      let t2 = x - 1 in
      loop n t2
  ;;
  let make n = loop n;;
  let result = make 42 3;;
  $ ./interp_check.exe capture.ml
  result = 42
  $ ../driver/compiler.exe --stop-after riscv -o capture.s capture.ml

Mutually recursive functions receive the captures of the whole group

  $ cat > mutual.ml <<EOF
  > let make a b =
  >   let rec even n = if n = 0 then a else odd (n - 1)
  >   and odd n = if n = 0 then b else even (n - 1) in
  >   even
  > ;;
  > let result = make 40 42 3
  > EOF
  $ ../driver/compiler.exe --stop-after ll mutual.ml
  let rec even a b n =
    let t1 = n = 0 in
    if t1
    then a
    else
      let t2 = n - 1 in
      odd a b t2
  and odd a b n =
    let t3 = n = 0 in
    if t3
    then b
    else
      let t4 = n - 1 in
      even a b t4
  ;;
  let make a b = even a b;;
  let result = make 40 42 3;;
  $ ./interp_check.exe mutual.ml
  result = 42
  $ ../driver/compiler.exe --stop-after riscv -o mutual.s mutual.ml

Parameters and local bindings preserve the captured value

  $ cat > shadow.ml <<EOF
  > let make k =
  >   let rec f n =
  >     if n = 0 then k else let k = n + 1 in f (n - 1) + k
  >   in f
  > ;;
  > let result = make 1 2
  > EOF
  $ ./interp_check.exe shadow.ml
  result = 6
  $ ../driver/compiler.exe --stop-after riscv -o shadow.s shadow.ml

  $ cat > parameter.ml <<EOF
  > let make k =
  >   let rec f k = k + 1 and g x = k + f x in g
  > ;;
  > let result = make 40 1
  > EOF
  $ ./interp_check.exe parameter.ml
  result = 42
  $ ../driver/compiler.exe --stop-after riscv -o parameter.s parameter.ml

Lifting a local print_int preserves the standard function outside its scope

  $ cat > primitive.ml <<EOF
  > let make base =
  >   let rec print_int n = if n = 0 then base else print_int (n - 1) in
  >   print_int
  > ;;
  > let result = make 42 3
  > EOF
  $ ../driver/compiler.exe --stop-after ll primitive.ml
  let rec print_int__ll1 base n =
    let t1 = n = 0 in
    if t1
    then base
    else
      let t2 = n - 1 in
      print_int__ll1 base t2
  ;;
  let make base = print_int__ll1 base;;
  let result = make 42 3;;
  $ ./interp_check.exe primitive.ml
  result = 42
  $ ../driver/compiler.exe --stop-after riscv -o primitive.s primitive.ml

The output of lambda lifting parses back

  $ ../driver/compiler.exe -parsing topdown --stop-after ll fac_cps.ml > ll_fac.ml
  $ ../driver/compiler.exe -parsing topdown --stop-after parsing ll_fac.ml > /dev/null
