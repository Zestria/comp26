let rec fib_cps n k =
  if n <= 1
  then k n
  else fib_cps (n - 1) (fun a -> fib_cps (n - 2) (fun b -> k (a + b)))
;;

let result = fib_cps 10 (fun x -> x);;
let () = print_endline (string_of_int result)
