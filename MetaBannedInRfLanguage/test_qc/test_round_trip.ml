[@@@ocaml.text "/*"]

(** Copyright 2026, Migunova Anastasia *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

type parser =
  { name : string
  ; parse : string -> (Ast.structure, string) result
  }

let top_down : parser = { name = "top-down"; parse = Rd_parser.parse_structure }
let bottom_up : parser = { name = "menhir"; parse = Menhir_parser.parse_structure }

let arb_structure = Ast_gen.arb_structure

let test_roundtrip (p : parser) =
  QCheck.Test.make
    ~name:(p.name ^ " round-trip: parse (print s) = s")
    ~count:300
    arb_structure
    (fun s ->
       match p.parse (Pprint.string_of_structure s) with
       | Ok s' -> s = s'
       | Error _ -> false)
;;

let () =
  let tests = [ test_roundtrip top_down; test_roundtrip bottom_up ] in
  exit (QCheck_runner.run_tests ~verbose:true tests)
;;
