let rec fib n k =
  if n <= 1 then k n else fib (n - 1) (fun a -> fib (n - 2) (fun b -> k (a + b)))
;;

fib 10 print_int
