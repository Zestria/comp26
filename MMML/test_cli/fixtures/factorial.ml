let rec fac n k =
  if n <= 1 then k 1
  else fac (n - 1) (fun r -> k (n * r))
let answer = fac 5 (fun x -> x)
