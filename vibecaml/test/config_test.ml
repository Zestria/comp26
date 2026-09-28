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
    expect_ok [ "--parser"; "menhir"; "--ext-glued-keywords"; "--dump-ast"; "fack.ml" ]
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
  expect_error [ "--parser" ];
  expect_error [ "--parser"; "lalr" ];
  expect_error [ "--unknown" ];
  expect_error [ "" ];
  expect_error [ "a.ml"; "b.ml" ]
;;
