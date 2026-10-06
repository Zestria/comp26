[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Vibecaml

let fail message =
  prerr_endline message;
  exit 1
;;

let parse arguments = Config.parse_arguments (Array.of_list ("vibecamlc" :: arguments))

let expect_ok arguments =
  match parse arguments with
  | Ok config -> config
  | Error message -> fail (String.concat " " arguments ^ ": " ^ message)
;;

let expect_error arguments =
  match parse arguments with
  | Ok _ -> fail (String.concat " " arguments ^ ": ожидалась ошибка")
  | Error _ -> ()
;;

let () =
  assert (Config.parse_arguments [||] = Ok Config.default);
  assert (expect_ok [] = Config.default);
  let config =
    expect_ok [ "-parsing"; "menhir"; "--ext-glued-keywords"; "--dump-ast"; "fack.ml" ]
  in
  assert (config.parser = Config.Menhir);
  assert config.extensions.glued_keywords;
  assert (not config.extensions.operator_declarations);
  assert config.dump_ast;
  assert (not config.dump_pretty_printed);
  assert (config.input_file = Some "fack.ml");
  let config = expect_ok [ "prog.ml"; "--ext-operator-declarations"; "--dump-pp" ] in
  assert (config.parser = Config.Recursive_descent);
  assert config.extensions.operator_declarations;
  assert config.dump_pretty_printed;
  assert (config.input_file = Some "prog.ml");
  assert ((expect_ok [ "-" ]).input_file = Some "-");
  assert ((expect_ok [ "-parsing"; "topdown" ]).parser = Config.Recursive_descent);
  expect_error [ "-parsing" ];
  expect_error [ "-parsing"; "lalr" ];
  (* Старый флаг заменён флагом из ТЗ. *)
  expect_error [ "--parser"; "menhir" ];
  (* Без --stop-after работает весь конвейер. *)
  assert (Config.default.stop_after = None);
  let stop_after value = (expect_ok [ "--stop-after"; value ]).stop_after in
  assert (stop_after "parsing" = Some Config.Parsing);
  assert (stop_after "cc" = Some Config.Closure_conversion);
  assert (stop_after "ll" = Some Config.Lambda_lifting);
  assert (stop_after "anf" = Some Config.Anf);
  expect_error [ "--stop-after" ];
  expect_error [ "--stop-after"; "typing" ];
  (* Флаги дампа печатают результат разбора, с другими стадиями они не
     сочетаются. *)
  assert (expect_ok [ "--dump-ast"; "--stop-after"; "parsing" ]).dump_ast;
  expect_error [ "--dump-ast"; "--stop-after"; "ll" ];
  expect_error [ "--stop-after"; "anf"; "--dump-pp" ];
  assert (Config.default.output_file = None);
  assert ((expect_ok [ "-o"; "out.ml" ]).output_file = Some "out.ml");
  assert ((expect_ok [ "-o"; "-"; "prog.ml" ]).output_file = Some "-");
  assert ((expect_ok [ "-o"; "-"; "prog.ml" ]).input_file = Some "prog.ml");
  expect_error [ "-o" ];
  expect_error [ "-o"; "--dump-ast" ];
  expect_error [ "-o"; "" ];
  expect_error [ "-o"; "a.ml"; "-o"; "b.ml" ];
  expect_error [ "--unknown" ];
  expect_error [ "" ];
  expect_error [ "a.ml"; "b.ml" ]
;;
