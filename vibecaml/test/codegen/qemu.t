Настоящие исходники проходят весь конвейер и запускаются без libc.
Сравнение всего stdout также ловит изменение порядка вычисления эффектов.

Рантайм сам возвращает unit после печати; кодоген не должен его исправлять.

  $ sh ../../scripts/run.sh abi/print_unit.s
  1230

Граничные значения print_int: ноль, отрицательное и границы 64-битного слова.

  $ sh ../../scripts/run.sh abi/print_edges.s
  0
  -5
  9223372036854775807
  -9223372036854775808

  $ run=../../scripts/run.sh
  $ for input in ../middleend/*.src simple.src operators.src calls.src tuple_boundary.src; do
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

Проверяем обе аллокации ровно на границе небольшой тестовой кучи.
Страница mmap может быть больше указанного размера: сработать должна
проверка vc_heap_end, а не случайный SIGSEGV за страницей.

  $ sed 's/^\.equ HEAP_SIZE, .*/.equ HEAP_SIZE, 48/' ../../runtime/runtime.s > tiny-runtime.s
  $ sh "$run" abi/heap_boundary.s tiny-runtime.s > out.txt 2> err.txt
  [1]
  $ cat out.txt
  10
  0
  $ cat err.txt
  Error: not enough memory on heap

Два кортежа по 16 байт помещаются в 32 байта, третий уже не должен создаться.

  $ sed 's/^\.equ HEAP_SIZE, .*/.equ HEAP_SIZE, 32/' ../../runtime/runtime.s > tiny-runtime.s
  $ vibecamlc tuple_boundary.src -o tuple-boundary.s
  $ sh "$run" tuple-boundary.s tiny-runtime.s > out.txt 2> err.txt
  [1]
  $ cat out.txt
  37
  $ cat err.txt
  Error: not enough memory on heap

Большие CPS-примеры исчерпывают фиксированную кучу и завершаются с ошибкой,
а не продолжают запись за её границей. Здесь оба парсера идут до машинного кода.

  $ sed 's/fibk 10 /fibk 25 /' fibk.src > big-fib.ml
  $ sed 's/fack 5 /fack 40000 /' fack.src > big-fack.ml
  $ for parser in topdown menhir; do
  >   for input in big-fib.ml big-fack.ml; do
  >     vibecamlc -parsing "$parser" "$input" -o big.s || exit 1
  >     sh "$run" big.s > out.txt 2> err.txt && exit 1
  >     test "$?" -eq 1 || exit 1
  >     test ! -s out.txt || exit 1
  >     echo 'Error: not enough memory on heap' > expected.txt
  >     diff expected.txt err.txt || exit 1
  >   done
  > done
