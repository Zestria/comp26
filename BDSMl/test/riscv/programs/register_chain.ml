let chain x =
  let a = x + 1 in
  let b = a + 1 in
  let c = b + 1 in
  let d = c + 1 in
  let e = d + 1 in
  let f = e + 1 in
  let g = f + 1 in
  let h = g + 1 in
  let i = h + 1 in
  let j = i + 1 in
  let k = j + 1 in
  k + 1
let () = print_int (chain 1)
