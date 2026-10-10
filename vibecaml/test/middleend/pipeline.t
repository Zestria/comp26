Сквозная проверка на обычных исходниках: функции захватывают локальные
переменные, имена затеняются, операторы могут быть переопределены.
Сравнивается весь stdout, поэтому порядок эффектов тоже проверяется.
Предупреждения 5 и 26 отключены: отбрасывание функции и сохранение вычислений
с эффектами намеренно оставляют частичные применения и неиспользуемые let.
Ошибки OCaml остаются видны и останавливают тест.

  $ for input in *.src; do
  >   ocaml -w -5-26 "$input" > expected.txt || exit 1
  >   for parser in topdown menhir; do
  >     for stage in rename cc ll anf; do
  >       vibecamlc -parsing "$parser" --stop-after "$stage" "$input" -o result.ml || exit 1
  >       ocaml -w -5-26 result.ml > actual.txt || exit 1
  >       diff expected.txt actual.txt || exit 1
  >     done
  >     vibecamlc -parsing "$parser" "$input" > default.ml || exit 1
  >     diff result.ml default.ml || exit 1
  >   done
  >   printf '%s: ' "$input"; cat expected.txt; echo
  > done
  branch.src: 0
  builtin_shadow.src: 5
  capture.src: 12
  curried.src: 66654151415
  deep.src: 6
  discarded.src: 5
  effects.src: 21343
  fack.src: 120
  fibk.src: 55
  groups.src: 8
  local_fack.src: 120
  local_fibk.src: 55
  local_group.src: 11
  local_mutual.src: 110
  mutual.src: 1
  operator.src: 7
  shadow.src: 20
  siblings.src: 12
  tuple.src: 10
  unary_minus.src: -2
  user_fresh.src: 8
