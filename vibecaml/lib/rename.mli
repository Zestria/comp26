[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Renaming: модифицирует AST таким образом, чтобы имена всех связанных вхождений были различны *)
val rename : Ast.structure -> (Ast.structure, string) result
