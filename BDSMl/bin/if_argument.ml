let id x = x;;
let result = id (if true then 9 else 0);;
let () = print_endline (string_of_int result)
