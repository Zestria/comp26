[@@@ocaml.text "/*"]

(** Copyright 2023-2024, Kakadu, bogban893, shamashaehmitov and garry2311 *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type stage =
  | Parsing
  | Alpha
  | Closure_conv
  | Lambda_lift
  | Anf

let run ~stop_after:_ _ = failwith "Pipeline.run: not implemented"
