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

let run ~stop_after program =
  let open BDSMl_frontend in
  match stop_after with
  | Parsing -> Printer.print_program program
  | Alpha ->
    Ast_passes.Alpha.alpha program |> Printer.print_program
  | Closure_conv ->
    Ast_passes.Closure_conv.closure_conv program
    |> Printer.print_program
  | Lambda_lift ->
    Ast_passes.Alpha.alpha program
    |> Ast_passes.Closure_conv.closure_conv
    |> Ast_passes.Lambda_lift.lambda_lift
    |> Printer.print_program
  | Anf ->
    Ast_passes.Alpha.alpha program
    |> Ast_passes.Closure_conv.closure_conv
    |> Ast_passes.Lambda_lift.lambda_lift
    |> Anf.Anf_conv.anf
    |> Anf.Anf_pp.anf_pp
;;
