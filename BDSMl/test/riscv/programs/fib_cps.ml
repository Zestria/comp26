let rec fib_cps n k =
  if n <= 1
  then k n
  else fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
;;

let () = print_int (fib_cps 10 (fun x -> x))
