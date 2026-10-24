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

The output of lambda lifting parses back

  $ ../driver/compiler.exe -parsing topdown --stop-after ll fac_cps.ml > ll_fac.ml
  $ ../driver/compiler.exe -parsing topdown --stop-after parsing ll_fac.ml > /dev/null
