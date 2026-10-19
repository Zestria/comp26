
  $ cat > closure.ml <<'ML'
  > let make = fun x -> let y = x + 1 in fun z -> y + z in
  > let f = make 10 in
  > f 2
  > ML
  $ ../compiler.exe --stop-after closure -o - closure.ml
  let cc_fun0 =
    (fun x ->
       let y = x + 1 in
       let cc_fun1 = (fun cc_capture0 z -> cc_capture0 + z) in
       cc_fun1 y)
  in
  let make = cc_fun0 in
  let f = make 10 in
  f 2


  $ ../compiler.exe --stop-after closure -parsing topdown -o topdown.ml closure.ml
  $ ../compiler.exe --stop-after closure -parsing menhir -o menhir.ml closure.ml
  $ cmp topdown.ml menhir.ml
  $ ../compiler.exe --stop-after closure -o - < closure.ml > stdin.ml
  $ cmp topdown.ml stdin.ml


  $ ../compiler.exe --stop-after anf closure.ml > anf.ml
  $ ../compiler.exe --stop-after closure anf.ml > from-anf.ml
  $ cmp topdown.ml from-anf.ml

The output is ordinary OCaml, accepted by the OCaml compiler and both parsers.

  $ ocamlc -c topdown.ml
  $ ../compiler.exe --stop-after parsing -parsing topdown topdown.ml > /dev/null
  $ ../compiler.exe --stop-after parsing -parsing menhir topdown.ml > /dev/null
