let x = 5;;
let result =
  let x = x + 1 in
  let x = x + 1 in
  (fun x -> x + 1) x
;;

let () = print_endline (string_of_int result)
