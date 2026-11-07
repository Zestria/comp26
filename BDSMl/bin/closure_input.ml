(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let f y =
  let rec loop i = if i = 0 then y else loop (i - 1) in
  loop 10

let main = print_endline (string_of_int (f 7))
