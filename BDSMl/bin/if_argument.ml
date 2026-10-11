(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let id x = x;;
let result = id (if true then 9 else 0);;
let () = print_endline (string_of_int result)
