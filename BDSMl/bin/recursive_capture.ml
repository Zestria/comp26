let rec loop n = if n = 0 then 0 else (fun x -> loop x) (n - 1);;
let () = print_endline (string_of_int (loop 4))
