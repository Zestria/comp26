(** Генерация GNU-совместимого ассемблера RV64 из ANF.
    Пока возвращает диагностическую ошибку: backend ещё не реализован. *)
val generate : BDSMl_middleend.Anf.Anf_ast.aprogram -> (string, string) result
