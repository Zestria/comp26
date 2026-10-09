[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Closure conversion: модифицирует программу таким образом,
  чтобы у каждой `fun` не было свободных переменных кроме глобальных *)
val convert : Ast.structure -> (Ast.structure, string) result
