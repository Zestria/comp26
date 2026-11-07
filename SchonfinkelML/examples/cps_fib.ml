let rec fib n k =
  if n < 2 then k n else fib (n - 1) (fun x -> fib (n - 2) (fun y -> k (x + y)))
in
let n = fib 6 (fun x -> x) in
print_int n
