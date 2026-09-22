(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(** Нисходящий синтаксический анализатор подмножества OCaml.

    Ошибка --- строка вида ["строка:колонка: описание"], колонка считается с
    единицы. *)

include Parser_intf.S
