Совместная работа парсеров, rename, closure conversion, lambda lifting, ANF
и упрощения на исходных CPS-программах с захватами. Запуск через OCaml
проверяет, что весь конвейер сохраняет результат программы.

  $ cat > fack.ml <<'ML'
  > let rec fack n k =
  >   if n <= 1 then k 1 else fack (n - 1) (fun m -> k (m * n))
  > let identity x = x
  > let () = print_int (fack 5 identity)
  > ML

  $ cat > fibk.ml <<'ML'
  > let rec fibk n k =
  >   if n < 2 then k n
  >   else fibk (n - 1)
  >     (fun a -> fibk (n - 2) (fun b -> k (a + b)))
  > let identity x = x
  > let () = print_int (fibk 10 identity)
  > ML

Для каждой стадии результат пишется через -o, затем исполняется.

  $ for sample in fack fibk; do
  >   ocaml "$sample.ml" > expected.txt || exit 1
  >   for parser in topdown menhir; do
  >     for stage in parsing rename cc ll anf; do
  >       vibecamlc -parsing "$parser" --stop-after "$stage" "$sample.ml" -o output.ml || exit 1
  >       ocaml output.ml > actual.txt || exit 1
  >       diff expected.txt actual.txt || exit 1
  >     done
  >   done
  >   cat expected.txt; echo
  > done
  120
  55
