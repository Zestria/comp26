let f y =
  let rec loop i = if i = 0 then y else loop (i - 1) in
  loop 10

let main = print_endline (string_of_int (f 7))
