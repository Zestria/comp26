type token =
  | TUnit
  | TInt of int
  | TIdent of string
  | TTrue
  | TFalse
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TEqual
  | TNotEqual
  | TMore
  | TLess
  | TEMore
  | TELess
  | TAnd
  | TOr
  | TNot
  | TLParen
  | TRParen
  | TUnderscore
  | TSemisemi (* ;; *)
  | TEOF
  | TIF
  | TThen
  | TElse
  | TLet
  | TRec
  | TIn
  | TFun
  | TArrow (* -> *)

type error = [ `lexer_error of string ]

val tokenize : string -> (token list, [> error ]) result
