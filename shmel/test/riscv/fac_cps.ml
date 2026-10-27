let rec fac n k = if n <= 1 then k 1 else fac (n - 1) (fun r -> k (n * r))
let main = print_int (fac 6 (fun x -> x))
