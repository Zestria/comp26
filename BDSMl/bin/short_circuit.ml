(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let a = true || (1 / 0 = 0);;
let b = false && (1 / 0 = 0);;
let result = if a && not b then 42 else 0;;
let () = print_endline (string_of_int result)
