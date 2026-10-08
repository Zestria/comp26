%{
open Ast
open Tokens

let abstract parameters body =
  List.fold_right (fun parameter body -> Abs (parameter, body)) parameters body
%}

%token <int> TInt
%token <string> TIdent
%token TTrue TFalse TUnit
%token TLet TRec TIn TFun TArrow TIF TThen TElse
%token TPlus TMinus TStar TSlash
%token TEqual TNotEqual TMore TLess TEMore TELess
%token TAnd TOr TNot
%token TLParen TRParen TUnderscore TSemisemi TEOF

%nonassoc TIn
%nonassoc TArrow
%nonassoc TElse
%right TOr
%right TAnd
%left TEqual TNotEqual TMore TLess TEMore TELess
%left TPlus TMinus
%left TStar TSlash
%nonassoc UNARY

%start <Ast.program> program
%start <Ast.expr> expression

%%

program:
  | items = structure; TEOF { items }

expression:
  | body = expr; TEOF { body }

(* Expressions are separated by [;;]; let definitions may follow each other. *)
structure:
  | body = expr; rest = structure_tail { StrEval body :: rest }
  | items = structure_tail { items }

structure_tail:
  | { [] }
  | TSemisemi; rest = structure { rest }
  | binding = let_binding; rest = structure_tail
      { let rec_flag, pattern, body = binding in
        StrLet (rec_flag, pattern, body) :: rest }

let_binding:
  | TLet; rec_flag = rec_flag; name = pattern;
    parameters = list(pattern); TEqual; body = expr
      { rec_flag, name, abstract parameters body }

rec_flag:
  | { NonRec }
  | TRec { Rec }

pattern:
  | name = TIdent { PVar name }
  | TUnderscore { PUnderscore }
  | TUnit { PUnit }
  | TLParen; TRParen { PUnit }

expr:
  | binding = let_binding; TIn; body = expr %prec TIn
      { let rec_flag, pattern, value = binding in
        Let (rec_flag, pattern, value, body) }
  | TFun; parameters = nonempty_list(pattern); TArrow; body = expr %prec TArrow
      { abstract parameters body }
  | TIF; condition = expr; TThen; if_true = expr; TElse; if_false = expr %prec TElse
      { If (condition, if_true, if_false) }
  | left = expr; op = binop; right = expr { BinOp (op, left, right) }
  | TMinus; body = expr %prec UNARY { UnOp (Neg, body) }
  | TNot; body = expr %prec UNARY { UnOp (Not, body) }
  | body = application { body }

%inline binop:
  | TPlus { Plus }
  | TMinus { Minus }
  | TStar { Mult }
  | TSlash { Div }
  | TEqual { Equal }
  | TNotEqual { NotEqual }
  | TMore { More }
  | TLess { Less }
  | TEMore { EMore }
  | TELess { ELess }
  | TAnd { And }
  | TOr { Or }

application:
  | body = atom { body }
  | fn = application; argument = atom { App (fn, argument) }

atom:
  | value = TInt { Int value }
  | TTrue { Bool true }
  | TFalse { Bool false }
  | name = TIdent { Var name }
  | TUnit { Unit }
  | TLParen; TRParen { Unit }
  | TLParen; body = expr; TRParen { body }
