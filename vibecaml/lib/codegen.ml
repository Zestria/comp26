[@@@ocaml.text "/*"]

(** Copyright 2026, vibecaml team *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

[@@@ocaml.text "/*"]

open Anf
open Riscv

exception Unsupported of string

let error format =
  Printf.ksprintf (fun message -> raise (Unsupported ("Codegen: " ^ message))) format
;;

type function_symbol =
  { code : label
  ; closure : label
  ; arity : int
  }

type global =
  | Function of function_symbol
  | Value of label

type destination =
  | Result
  | Slot of int

let argument_registers = [ A0; A1; A2; A3; A4; A5; A6; A7 ]
let align16 bytes = (bytes + 15) / 16 * 16

let rec locals = function
  | ALet (name, rhs, body) -> name :: (action_locals rhs @ locals body)
  | ACExpr rhs -> action_locals rhs

and action_locals = function
  | CIf (_, yes, no) -> locals yes @ locals no
  | _ -> []
;;

let rec argument_space = function
  | ALet (_, rhs, body) -> max (action_space rhs) (argument_space body)
  | ACExpr rhs -> action_space rhs

and action_space = function
  | CApp (_, arguments) -> List.length arguments
  | CIf (_, yes, no) -> max (argument_space yes) (argument_space no)
  | _ -> 0
;;

let builtin_arity = function
  | "print_int" | "~-" -> 1
  | "+"
  | "-"
  | "*"
  | "/"
  | "mod"
  | "land"
  | "lor"
  | "lxor"
  | "lsl"
  | "lsr"
  | "asr"
  | "="
  | "<>"
  | "<"
  | "<="
  | ">"
  | ">="
  | "&&"
  | "||" -> 2
  | name -> error "неподдержанное имя или операция %S" name
;;

(* Индекс делает отображение инъективным даже для a' и a_, которые после
   очистки имеют одинаковую читаемую часть. Префикс отделяет код от runtime. *)
let symbol kind index name =
  let readable =
    String.map
      (function
        | ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_') as c -> c
        | _ -> '_')
      name
  in
  label (Printf.sprintf "vc_%s_%s_%d" kind readable index)
;;

(* Состояние выдачи одного запуска generate. Все списки обратные:
   новый элемент лежит в голове. *)
type state =
  { output : item list
  ; closures : function_symbol list
  ; wrappers : (string * function_symbol) list
  }

(* Состояние идёт последним аргументом, чтобы свёртки вставали в конвейер. *)
let fold step items state = List.fold_left (fun state item -> step item state) state items

let foldi step items state =
  let _, state =
    List.fold_left
      (fun (index, state) item -> index + 1, step index item state)
      (0, state)
      items
  in
  state
;;

(* Окружение кода одной функции: глобальные символы программы и слоты
   параметров и локальных имён относительно s0. *)
type env =
  { globals : (string * global) list
  ; slots : (string * int) list
  }

let emit ?comm instruction state =
  { state with output = Instr (instruction, comm) :: state.output }
;;

let mark name state = { state with output = Label name :: state.output }
let directive value state = { state with output = Directive value :: state.output }

let use_closure function_ state =
  let closures =
    if List.mem function_ state.closures
    then state.closures
    else function_ :: state.closures
  in
  function_.closure, { state with closures }
;;

let builtin name state =
  match List.assoc_opt name state.wrappers with
  | Some function_ -> function_, state
  | None ->
    let index = List.length state.wrappers in
    let function_ =
      { code = symbol "builtin" index name
      ; closure = symbol "builtin_closure" index name
      ; arity = builtin_arity name
      }
    in
    function_, { state with wrappers = (name, function_) :: state.wrappers }
;;

let slot env name =
  match List.assoc_opt name env.slots with
  | Some offset -> offset
  | None -> error "нет слота для %S" name
;;

let load env register value state =
  match value with
  | ImmNum n -> emit (Li (register, n)) state
  | ImmBool value -> emit (Li (register, if value then 1 else 0)) state
  | ImmUnit -> emit (Li (register, 0)) state
  | ImmId name ->
    (match List.assoc_opt name env.slots with
     | Some offset -> emit (Ld (register, offset, S0)) state
     | None ->
       (match List.assoc_opt name env.globals with
        | Some (Value address) ->
          state |> emit (La (register, address)) |> emit (Ld (register, 0, register))
        | Some (Function function_) ->
          let closure, state = use_closure function_ state in
          emit (La (register, closure)) state
        | None ->
          let function_, state = builtin name state in
          let closure, state = use_closure function_ state in
          emit (La (register, closure)) state))
;;

let save destination state =
  match destination with
  | Result -> state
  | Slot offset -> emit (Sd (A0, offset, S0)) state
;;

let fail_divisor state =
  let safe = fresh_label () in
  state
  |> emit (Bne (T1, Zero, safe))
  |> emit (Li (A0, 0))
  |> emit (Call (label "vc_fail"))
  |> mark safe
;;

let inline env name arguments state =
  match name, arguments with
  | "print_int", [ value ] ->
    state |> load env A0 value |> emit (Call (label "vc_print_int"))
  | "~-", [ value ] -> state |> load env T0 value |> emit (Neg (A0, T0))
  | _, [ left; right ] ->
    let state = state |> load env T0 left |> load env T1 right in
    (match name with
     | "+" -> emit (Add (A0, T0, T1)) state
     | "-" -> emit (Sub (A0, T0, T1)) state
     | "*" -> emit (Mul (A0, T0, T1)) state
     | "/" -> state |> fail_divisor |> emit (Div (A0, T0, T1))
     | "mod" -> state |> fail_divisor |> emit (Rem (A0, T0, T1))
     | "land" | "&&" -> emit (And (A0, T0, T1)) state
     | "lor" | "||" -> emit (Or (A0, T0, T1)) state
     | "lxor" -> emit (Xor (A0, T0, T1)) state
     | "lsl" -> emit (Sll (A0, T0, T1)) state
     | "lsr" -> emit (Srl (A0, T0, T1)) state
     | "asr" -> emit (Sra (A0, T0, T1)) state
     | "=" | "<>" ->
       state
       |> emit (Xor (A0, T0, T1))
       |> emit (if name = "=" then Seqz (A0, A0) else Snez (A0, A0))
     | "<" -> emit (Slt (A0, T0, T1)) state
     | ">" -> emit (Slt (A0, T1, T0)) state
     | "<=" -> state |> emit (Slt (A0, T1, T0)) |> emit (Seqz (A0, A0))
     | ">=" -> state |> emit (Slt (A0, T0, T1)) |> emit (Seqz (A0, A0))
     | _ -> error "неподдержанная операция %S" name)
  | _ -> error "неверная арность операции %S" name
;;

(* Переход на [otherwise], если условие ложно. *)
let branch_unless env otherwise condition state =
  match condition with
  | CondImm value -> state |> load env T0 value |> emit (Beq (T0, Zero, otherwise))
  | CondCmp (operator, left, right) ->
    let state = state |> load env T0 left |> load env T1 right in
    let branch =
      match operator with
      | "=" -> Bne (T0, T1, otherwise)
      | "<>" -> Beq (T0, T1, otherwise)
      | "<" -> Bge (T0, T1, otherwise)
      | "<=" -> Blt (T1, T0, otherwise)
      | ">" -> Bge (T1, T0, otherwise)
      | ">=" -> Blt (T0, T1, otherwise)
      | _ -> error "неподдержанное условие %S" operator
    in
    emit branch state
;;

let allocate_tuple env fields state =
  let bytes = 8 * List.length fields in
  if bytes > 2047 then error "кортеж слишком велик для inline-аллокации";
  let enough = fresh_label () in
  state
  |> emit (Mv (A0, S10))
  |> emit (Addi (T1, S10, bytes))
  |> emit (La (T2, label "vc_heap_end"))
  |> emit (Ld (T2, 0, T2))
  |> emit (Bge (T2, T1, enough))
  |> emit (Li (A0, 1))
  |> emit (Call (label "vc_fail"))
  |> mark enough
  |> emit (Mv (S10, T1))
  |> foldi
       (fun index value state ->
          state |> load env T0 value |> emit (Sd (T0, 8 * index, A0)))
       fields
;;

let load_field env value index size state =
  if size < 2 || index < 0 || index >= size || index > 255
  then error "неверное поле %d кортежа размера %d" index size;
  state |> load env T0 value |> emit (Ld (A0, 8 * index, T0))
;;

let application env function_ arguments state =
  let known =
    match function_ with
    | ImmId name when not (List.mem_assoc name env.slots) ->
      List.assoc_opt name env.globals
    | _ -> None
  in
  let intrinsic =
    match function_ with
    | ImmId name
      when (not (List.mem_assoc name env.slots)) && not (List.mem_assoc name env.globals)
      -> Some name
    | ImmId _ -> None
    | _ -> error "применение константы как функции"
  in
  match known, intrinsic with
  | None, Some name when List.length arguments = builtin_arity name ->
    inline env name arguments state
  | Some (Function function_), _ when List.length arguments = function_.arity ->
    state
    |> foldi
         (fun index value state ->
            if index < 8
            then load env (List.nth argument_registers index) value state
            else state |> load env T0 value |> emit (Sd (T0, 8 * (index - 8), Sp)))
         arguments
    |> emit (Call function_.code)
  | _ ->
    (* argv лежит в заранее выделенной нижней части кадра. Вызов
       может испортить t*/a*, но живые значения остаются в слотах. *)
    state
    |> foldi
         (fun index value state ->
            state |> load env T0 value |> emit (Sd (T0, 8 * index, Sp)))
         arguments
    |> load env A0 function_
    |> emit (Li (A1, List.length arguments))
    |> emit (Mv (A2, Sp))
    |> emit (Call (label "vc_apply"))
;;

let rec expression env destination body state =
  match body with
  | ALet (name, rhs, body) ->
    let target = Slot (slot env name) in
    state |> action env target rhs |> expression env destination body
  | ACExpr rhs -> action env destination rhs state

and action env destination rhs state =
  match rhs with
  | CIf (test, yes, no) ->
    let otherwise = fresh_label () in
    let finish = fresh_label () in
    (* Обе ветки пишут в одно место. Общий хвост не копируем. *)
    state
    |> branch_unless env otherwise test
    |> expression env destination yes
    |> emit (J finish)
    |> mark otherwise
    |> expression env destination no
    |> mark finish
  | rhs ->
    let state =
      match rhs with
      | CImmExpr value -> load env A0 value state
      | CApp (function_, arguments) -> application env function_ arguments state
      | CTuple (first, second, rest) -> allocate_tuple env (first :: second :: rest) state
      | CField (value, index, size) -> load_field env value index size state
      | CIf _ -> error "условие попало в обработчик простого действия"
    in
    save destination state
;;

let prologue env code frame parameters state =
  state
  |> directive Text
  |> directive (Globl code)
  |> mark code
  |> emit ~comm:"кадр: ra, fp, локальные и исходящие аргументы" (Addi (Sp, Sp, -frame))
  |> emit (Sd (Ra, frame - 8, Sp))
  |> emit (Sd (S0, frame - 16, Sp))
  |> emit (Addi (S0, Sp, frame))
  |> foldi
       (fun index name state ->
          if index < 8
          then emit (Sd (List.nth argument_registers index, slot env name, S0)) state
          else (
            (* [slot] может бросить [error], поэтому он идёт после [Ld], как и
               прежде. *)
            let state = emit (Ld (T0, 8 * (index - 8), S0)) state in
            emit (Sd (T0, slot env name, S0)) state))
       parameters
;;

let compile_body env (body, global_result) state =
  let state = expression env Result body state in
  match global_result with
  | None -> state
  | Some address -> state |> emit (La (T0, address)) |> emit (Sd (A0, 0, T0))
;;

(* Эпилог общий, и здесь нет TCO: это отдельное задание. *)
let epilogue frame parameters state =
  let state =
    match parameters with
    | [] -> emit (Li (A0, 0)) state
    | _ -> state
  in
  state
  |> emit (Ld (Ra, frame - 8, Sp))
  |> emit (Ld (S0, frame - 16, Sp))
  |> emit (Addi (Sp, Sp, frame))
  |> emit Ret
;;

let compile_function globals code parameters bodies state =
  let local_names = parameters @ List.concat_map (fun (body, _) -> locals body) bodies in
  if List.length local_names <> List.length (List.sort_uniq String.compare local_names)
  then error "повторное локальное имя: нарушен контракт rename/CC";
  let slots = List.mapi (fun index name -> name, -24 - (8 * index)) local_names in
  let outgoing =
    List.fold_left (fun n (body, _) -> max n (argument_space body)) 0 bodies
  in
  let frame = align16 (16 + (8 * (List.length local_names + outgoing))) in
  (* И положительный addi эпилога, и все смещения должны влезть в imm12.
     Большие кадры отвергаем до передачи инструкций принтеру с assert. *)
  if frame > 2032 then error "кадр %d байт превышает поддерживаемые 2032" frame;
  let env = { globals; slots } in
  state
  |> prologue env code frame parameters
  |> fold (compile_body env) bodies
  |> epilogue frame parameters
;;

let generate program =
  try
    let bindings = List.concat_map snd program in
    let globals =
      List.mapi
        (fun index (name, parameters, _) ->
           let global =
             match parameters with
             | [] -> Value (symbol "value" index name)
             | _ ->
               Function
                 { code = symbol "fn" index name
                 ; closure = symbol "closure" index name
                 ; arity = List.length parameters
                 }
           in
           name, global)
        bindings
    in
    let names = List.map fst globals in
    if List.length names <> List.length (List.sort_uniq String.compare names)
    then error "повторное имя верхнего уровня: сначала нужен rename";
    let state =
      fold
        (fun (name, parameters, body) state ->
           match List.assoc name globals with
           | Function function_ ->
             compile_function globals function_.code parameters [ body, None ] state
           | Value _ -> state)
        bindings
        { output = []; closures = []; wrappers = [] }
    in
    (* Каждое значение вычисляется в порядке исходных объявлений. Хвост
       маленького ANF-блока сохраняет результат в его глобальный слот. *)
    let values =
      List.filter
        (function
          | _, [], _ -> true
          | _ -> false)
        bindings
    in
    let value_bodies =
      List.map
        (fun (name, _, body) ->
           match List.assoc name globals with
           | Value address -> body, Some address
           | Function _ -> error "ожидалось глобальное значение %S" name)
        values
    in
    let state = compile_function globals (label "vc_main") [] value_bodies state in
    let state =
      fold
        (fun (name, function_) state ->
           let parameters =
             List.init function_.arity (fun n -> "builtin_arg" ^ string_of_int n)
           in
           let body =
             ACExpr (CApp (ImmId name, List.map (fun name -> ImmId name) parameters))
           in
           compile_function globals function_.code parameters [ body, None ] state)
        (List.rev state.wrappers)
        state
    in
    let state =
      state
      |> directive Data
      |> directive (Align 3)
      |> fold
           (fun (_, global) state ->
              match global with
              | Value address -> state |> mark address |> directive (Dword 0)
              | Function _ -> state)
           globals
    in
    let state =
      fold
        (fun function_ state ->
           state
           |> mark function_.closure
           |> directive (Dword_label function_.code)
           |> directive (Dword function_.arity)
           |> directive (Dword 0))
        (List.rev state.closures)
        state
    in
    Ok (List.rev state.output)
  with
  | Unsupported message -> Error message
;;
