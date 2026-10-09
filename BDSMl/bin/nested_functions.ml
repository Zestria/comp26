let outer x = fun y -> fun z -> x + y + z;;
let result = outer 2 3 4;;
let () = print_endline (string_of_int result)
