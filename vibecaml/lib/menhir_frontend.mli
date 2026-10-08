[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

(** Восходящий парсер на menhir и ocamllex.

    Ошибка --- строка вида ["строка:колонка: описание"], колонка считается с
    единицы. *)

include Parser_intf.S
