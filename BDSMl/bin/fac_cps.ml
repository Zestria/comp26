let rec fact_cps n k =
  if n <= 1 then k 1 else fact_cps (n - 1) (fun m -> k (n * m))
;;

let result = fact_cps 5 (fun x -> x);;
let () = print_endline (string_of_int result)
