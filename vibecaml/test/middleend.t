Совместная работа парсеров, lambda lifting, ANF и упрощения. Вход уже замкнут,
имена локальных переменных уникальны: rename и closure conversion подключит
автор отдельной задачи. В отличие от отдельных снимков дерева, запуск через
OCaml проверяет, что весь конвейер сохраняет результат программы.

  $ cat > fack.ml <<'ML'
  > let rec fack n k =
  >   if n <= 1 then k 1 else fack (n - 1) ((fun k1 n1 m -> k1 (m * n1)) k n)
  > let identity x = x
  > let () = print_int (fack 5 identity)
  > ML

  $ cat > fibk.ml <<'ML'
  > let rec fibk n k =
  >   if n < 2 then k n
  >   else fibk (n - 1)
  >     ((fun k1 n1 a -> fibk (n1 - 2) ((fun k2 a2 b -> k2 (a2 + b)) k1 a)) k n)
  > let identity x = x
  > let () = print_int (fibk 10 identity)
  > ML

Для каждой стадии результат пишется через -o, затем исполняется. Режим без
--stop-after должен совпасть с ANF, включая свежие имена.

  $ for sample in fack fibk; do
  >   ocaml "$sample.ml" > expected.txt || exit 1
  >   for parser in topdown menhir; do
  >     for stage in parsing ll anf; do
  >       vibecamlc -parsing "$parser" --stop-after "$stage" "$sample.ml" -o output.ml || exit 1
  >       ocaml output.ml > actual.txt || exit 1
  >       diff expected.txt actual.txt || exit 1
  >     done
  >     vibecamlc -parsing "$parser" "$sample.ml" > default.ml || exit 1
  >     diff output.ml default.ml || exit 1
  >   done
  >   cat expected.txt; echo
  > done
  120
  55
