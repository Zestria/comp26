
  $ cat > closure.ml <<'ML'
  > let make = fun x -> let y = x + 1 in fun z -> y + z in
  > let f = make 10 in
  > f 2
  > ML
  $ ../compiler.exe --stop-after closure -o - closure.ml
  let make = closure cc_code0 [] {
               code (cc_self0; x) captures [] =
                 let y = x + 1 in
                 closure cc_code1 [y] {
                   code (cc_self1; z) captures [y] =
                     let y = env(cc_self1)[0] in
                     y + z
                 }
             } in
  let f = apply(make; 10) in
  apply(f; 2)


  $ ../compiler.exe --stop-after closure -parsing topdown -o topdown.ir closure.ml
  $ ../compiler.exe --stop-after closure -parsing menhir -o menhir.ir closure.ml
  $ cmp topdown.ir menhir.ir
  $ ../compiler.exe --stop-after closure -o - < closure.ml > stdin.ir
  $ cmp topdown.ir stdin.ir


  $ ../compiler.exe --stop-after anf closure.ml > anf.ml
  $ ../compiler.exe --stop-after closure anf.ml > from-anf.ir
  $ cmp topdown.ir from-anf.ir
