Настоящие исходники проходят весь конвейер и запускаются без libc.
Сравнение всего stdout также ловит изменение порядка вычисления эффектов.

Рантайм сам возвращает unit после печати; кодоген не должен его исправлять.

  $ sh ../../scripts/run.sh abi/print_unit.s
  1230

  $ run=../../scripts/run.sh
  $ for input in ../middleend/*.src simple.src operators.src calls.src; do
  >   ocaml -w -5-26 "$input" > expected.txt || exit 1
  >   for parser in topdown menhir; do
  >     vibecamlc -parsing "$parser" "$input" -o generated.s || exit 1
  >     vibecamlc -parsing "$parser" --stop-after asm "$input" > explicit.s || exit 1
  >     diff generated.s explicit.s || exit 1
  >     sh "$run" generated.s > actual.txt || exit 1
  >     diff expected.txt actual.txt || exit 1
  >   done
  > done

Деление на ноль должно завершить процесс с кодом 1 и сообщением в stderr.

  $ echo 'let () = print_int (1 / 0)' > zero.ml
  $ vibecamlc zero.ml -o zero.s
  $ sh "$run" zero.s > out.txt 2> err.txt
  [1]
  $ cat out.txt
  $ cat err.txt
  Error: division by zero

Inline-аллокация кортежей проверяет конец кучи так же, как vc_closure.

  $ cat > heap.ml <<'ML'
  > let consume pair = let (a, b) = pair in a + b
  > let rec loop n =
  >   if n = 0 then 0 else
  >     let pair = (n, n) in
  >     let seen = consume pair in
  >     if seen < 0 then 0 else loop (n - 1)
  > let () = print_int (loop 70000)
  > ML
  $ vibecamlc heap.ml -o heap.s
  $ sh "$run" heap.s > out.txt 2> err.txt
  [1]
  $ cat err.txt
  Error: not enough memory on heap

Ручной ABI-драйвер проверяет нечётные арности, аргументы на стеке,
частичное и избыточное применение, не полагаясь на оптимизации C-компилятора.

  $ sh "$run" abi/stack.s
  0
  0
  9
  0
  9
