let rec fact n k = if n <= 1 then k 1 else fact (n - 1) (fun r -> k (r * n));;

fact 10 print_int
