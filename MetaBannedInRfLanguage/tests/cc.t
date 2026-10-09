CPS factorial and fibonacci after closure conversion

  $ cat > fac_cps.ml <<EOF
  > let rec fac_cps n k =
  >   if n <= 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))
  > ;;
  > let main = fac_cps 5 (fun x -> x)
  > EOF

  $ ../compiler.exe -parsing topdown --stop-after cc fac_cps.ml
  let rec fac_cps n k =
    let t1 = n <= 1 in
    if t1
    then k 1
    else
      let t2 = n - 1 in
      let t3__cc1 k n p =
        let t4 = p * n in
        k t4
      in
      let t3 = t3__cc1 k n in
      fac_cps t2 t3
  ;;
  let main =
    let t5 x = x in
    fac_cps 5 t5
  ;;

  $ cat > fib_cps.ml <<EOF
  > let rec fib_cps n k =
  >   match n with
  >   | 0 -> k 0
  >   | 1 -> k 1
  >   | n -> fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
  > ;;
  > let main = fib_cps 10 (fun x -> x)
  > EOF

  $ ../compiler.exe -parsing topdown --stop-after cc fib_cps.ml
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
        let t4__cc2 fib_cps k n a =
          let t5 = n - 2 in
          let t6__cc1 a k b =
            let t7 = a + b in
            k t7
          in
          let t6 = t6__cc1 a k in
          fib_cps t5 t6
        in
        let t4 = t4__cc2 fib_cps k n in
        fib_cps t3 t4
  ;;
  let main =
    let t8 x = x in
    fib_cps 10 t8
  ;;

The output of closure conversion parses back

  $ ../compiler.exe -parsing topdown --stop-after cc fac_cps.ml > cc_fac.ml
  $ ../compiler.exe -parsing topdown --stop-after parsing cc_fac.ml > /dev/null
