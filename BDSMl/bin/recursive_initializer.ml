(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let main =
  let rec constant = 1 in
  let rec f =
    let _initial = print_int 8 in
    let x = constant in
    fun n -> if n = 0 then x else f (n - 1)
  in
  let () = print_int (f 2) in
  print_int (f 3)

let rec g =
  let _initial = print_int 9 in
  let x = 2 in
  fun n -> if n = 0 then x else g (n - 1)

let () = print_int (g 2)
