CPS factorial in ANF

  $ cat > fac_cps.ml <<EOF
  > let rec fac_cps n k =
  >   if n <= 1 then k 1 else fac_cps (n - 1) (fun p -> k (p * n))
  > ;;
  > let main = fac_cps 5 (fun x -> x)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after anf fac_cps.ml
  let rec fac_cps n k =
    let t1 = n <= 1 in
    if t1
    then k 1
    else
      let t2 = n - 1 in
      let t3 p =
        let t4 = p * n in
        k t4
      in
      fac_cps t2 t3
  ;;
  let main =
    let t5 x = x in
    fac_cps 5 t5
  ;;

CPS fibonacci in ANF

  $ cat > fib_cps.ml <<EOF
  > let rec fib_cps n k =
  >   match n with
  >   | 0 -> k 0
  >   | 1 -> k 1
  >   | n -> fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
  > ;;
  > let main = fib_cps 10 (fun x -> x)
  > EOF

  $ ../driver/compiler.exe -parsing topdown --stop-after anf fib_cps.ml
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
        let t4 a =
          let t5 = n - 2 in
          let t6 b =
            let t7 = a + b in
            k t7
          in
          fib_cps t5 t6
        in
        fib_cps t3 t4
  ;;
  let main =
    let t8 x = x in
    fib_cps 10 t8
  ;;

Both parsers give the same ANF

  $ ../driver/compiler.exe -parsing topdown --stop-after anf fac_cps.ml > topdown.txt
  $ ../driver/compiler.exe -parsing menhir --stop-after anf fac_cps.ml > menhir.txt
  $ diff topdown.txt menhir.txt
  $ ../driver/compiler.exe -parsing topdown --stop-after anf fib_cps.ml > topdown.txt
  $ ../driver/compiler.exe -parsing menhir --stop-after anf fib_cps.ml > menhir.txt
  $ diff topdown.txt menhir.txt

-o writes ANF to a file, -o - to stdout

  $ ../driver/compiler.exe --stop-after anf fac_cps.ml > fac_stdout.txt
  $ ../driver/compiler.exe --stop-after anf -o fac_anf.ml fac_cps.ml
  $ diff fac_anf.ml fac_stdout.txt
  $ ../driver/compiler.exe --stop-after anf -o - fac_cps.ml | diff - fac_stdout.txt

The printed ANF is a valid program and stays the same after one more round

  $ ../driver/compiler.exe --stop-after anf fac_anf.ml > fac_anf2.ml
  $ diff fac_anf.ml fac_anf2.ml
