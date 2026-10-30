let rec fack n k = if n = 0 then k 1 else fack (n - 1) (fun v -> k (n * v)) in
fack 5 (fun x -> x)
