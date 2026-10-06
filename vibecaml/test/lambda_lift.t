Lambda lifting. Пока rename и closure conversion (#10) не подключены, на вход
подаются программы, замкнутые вручную: у вложенных функций нет свободных
локальных переменных, имена различны.

CPS-факториал после closure conversion: анонимная функция получает свободные
переменные лишними аргументами.

  $ cat > fack.ml <<'ML'
  > let rec fack n k =
  >   if n <= 1 then k 1 else fack (n - 1) ((fun k1 n1 m -> k1 (m * n1)) k n)
  > ML

  $ vibecamlc --stop-after ll fack.ml
  let rec lifted__1 k1 n1 m = k1 (m * n1) and fack n k = if n <= 1 then k 1 else fack (n - 1) (lifted__1 k n)

Вынесенная функция стоит в той же рекурсивной группе: в fibk вложенная функция
сама вызывает fibk. Вложенная выносится раньше внешней.

  $ cat > fibk.ml <<'ML'
  > let rec fibk n k =
  >   if n < 2
  >   then k n
  >   else
  >     fibk
  >       (n - 1)
  >       ((fun k1 n1 a -> fibk (n1 - 2) ((fun k2 a2 b -> k2 (a2 + b)) k1 a)) k n)
  > ML

  $ vibecamlc --stop-after ll fibk.ml
  let rec lifted__1 k2 a2 b = k2 (a2 + b) and lifted__2 k1 n1 a = fibk (n1 - 2) (lifted__1 k1 a) and fibk n k = if n < 2 then k n else fibk (n - 1) (lifted__2 k n)

Без --stop-after работает весь подключённый конвейер до упрощённого ANF.

  $ vibecamlc fack.ml > default.ml
  $ vibecamlc --stop-after anf fack.ml > anf.ml
  $ diff default.ml anf.ml
  $ echo 'let () = print_int (fack 5 (fun x -> x))' >> default.ml
  $ ocaml default.ml
  120

Проверка смысла: результат запускается настоящим ocaml и печатает то же, что
исходник.

  $ check () {
  >   cp "$1" source.ml && echo "$2" >> source.ml
  >   vibecamlc --stop-after ll "$1" > lifted.ml && echo "$2" >> lifted.ml
  >   ocaml source.ml && echo && ocaml lifted.ml && echo
  > }

  $ check fack.ml 'let () = print_int (fack 5 (fun x -> x))'
  120
  120

  $ check fibk.ml 'let () = print_int (fibk 10 (fun x -> x))'
  55
  55

Локальные функции выносятся под своим именем, локальные значения остаются на
месте. Функции в кортеже, в аргументе и справа от `let _` или кортежного
паттерна получают свежее имя. Вложенные функции выносятся раньше внешних.

  $ cat > shapes.ml <<'ML'
  > let _ = fun x -> x
  > let (twice, base) = ((fun t -> t * 2), 10)
  > let apply f v = f v
  > let main u =
  >   let inc = fun i -> i + 1 in
  >   let start = inc u in
  >   let outer = fun o -> (fun p -> (fun q -> q * 3) p + 1) o in
  >   apply (fun w -> w - 1) (outer (twice (start + base)))
  > ML

  $ vibecamlc --stop-after ll shapes.ml
  let lifted__1 x = x
  let _ = lifted__1
  let lifted__2 t = t * 2
  let twice, base = lifted__2, 10
  let apply f v = f v
  let inc i = i + 1
  let lifted__3 q = q * 3
  let lifted__4 p = lifted__3 p + 1
  let outer o = lifted__4 o
  let lifted__5 w = w - 1
  let main u = let start = inc u in apply lifted__5 (outer (twice (start + base)))

  $ check shapes.ml 'let () = print_int (main 4)'
  90
  90

Локальный `let rec` не поддерживается: его отвергает уже closure conversion,
lambda lifting страхует.

  $ cat > local_rec.ml <<'ML'
  > let f n = let rec loop i = if i > n then 0 else loop (i + 1) in loop 0
  > ML

  $ vibecamlc --stop-after ll local_rec.ml
  vibecaml: локальный let rec не поддерживается
  [1]

Closure conversion — отдельная задача участника команды. Нельзя выдавать
неизменённый исходник за результат этого прохода.

  $ vibecamlc --stop-after cc fack.ml
  vibecaml: стадия cc пока не подключена: ожидаются rename и closure conversion
  [1]

В `let rec` верхнего уровня справа должна стоять функция: иначе после выноса
в группе окажется не функция, а OCaml такое не принимает.

  $ echo 'let rec f = fun x -> x and v = (fun y -> y) 1' > rec_value.ml

  $ vibecamlc --stop-after ll rec_value.ml
  vibecaml: в let rec справа должна стоять функция
  [1]
