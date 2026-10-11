(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let mark n = let () = print_int n in n;;
let add x y = x + y;;

let first = add (mark 1) (mark 2);;
let second = (let () = print_int 3 in add) (mark 4) (mark 5);;
let third = mark 6 + mark 7;;
let () = print_endline (string_of_int (first + second + third))
