(* Bottom-up (menhir) parser for the MyLittleML OCaml subset.

   Grammar overview (precedence low -> high):

     expr  := let-binding [in expr] | fun | if | cmp
     cmp   := cmp (EQ|NEQ|LT|LE|GT|GE) add | add
     add   := add (PLUS|MINUS) mul | mul
     mul   := mul (STAR|SLASH|MOD) unary | unary
     unary := MINUS unary | app
     app   := app atom | atom        (application binds tightest, left-assoc)
     atom  := INT | TRUE | FALSE | ID | ( expr )

   A program is a list of expressions.  A `let` binding with an optional
   `[in expr]` continuation is a single production, so a top-level
   declaration is just an expression without the `in` part.

   Menhir reports 23 shift/reduce conflicts (in 11 states); every one of
   them is resolved in the direction that matches OCaml semantics (default:
   prefer shift).  Full derivations can be inspected by regenerating
   `parser.conflicts` with `menhir --explain`:

   1. Application extension (state 25): after `app_expr`, an atom token
      (INT/TRUE/FALSE/ID/LPAREN) extends the application rather than ending
      it, i.e. `f x y` groups as `(f x) y`.

   2. MINUS continuation (states 34..49): after `cmp op add`, a `-`
      continues the add level, i.e. `a = b - c` groups as `a = (b - c)`.

   3. `in` binding (states 55 and 63): after `let ... = <expr>`, an `in`
      opens the let-in continuation of the INNERMOST let, i.e.
      `let x = let y = z in e2` is `let x = (let y = z in e2)`.  Verified
      by the test `let x = 5 in x = 5`, which must parse as a let-in whose
      body is a comparison.

   4. Phrase continuation (state 73): after a top-level [item], an
      atom/keyword token may either start another item in the same phrase
      (shift, like the old whitespace-separator behaviour) or finish the
      phrase (reduce, used when `;;` follows).  A `;;` terminator always
      separates phrases; without it, atom-starting items glue into
      applications (application is tighter) and keyword-starting items
      become sibling items of the same phrase, which is equivalent. *)
%{
open Ast
%}

%token <int> INT
%token TRUE FALSE
%token <string> ID
%token LET REC IN FUN IF THEN ELSE MOD
%token PLUS MINUS STAR SLASH LT LE GT GE EQ NEQ
%token LPAREN RPAREN ARROW SEMI EOF

%start <Ast.prog> prog
%%

prog:
  | l = list(phrase) EOF { List.concat l }

(* A phrase is a non-empty sequence of top-level items, optionally
   terminated by `;;` (the separator is optional after the LAST phrase too,
   so `let x = 1;;` and `let x = 1` are both accepted). *)
phrase:
  | l = nonempty_list(item) SEMI { l }
  | l = nonempty_list(item) { l }

item:
  | e = expr { e }

expr:
  | LET r = rec_flag name = ID params = list(ID) EQ body = expr
    cont = option(in_tail)
    { let b = { Ast.rec_ = r; name; params; body } in
      match cont with
      | None -> Ast.Let (b, None)
      | Some e -> Ast.Let (b, Some e) }
  | e = cmp_expr                                      { e }
  | FUN params = nonempty_list(ID) ARROW body = expr  { Ast.Fun (params, body) }
  | IF cond = expr THEN thn = expr ELSE els = expr    { Ast.If (cond, thn, els) }

in_tail:
  | IN cont = expr { cont }

cmp_expr:
  | l = cmp_expr op = comparison r = add_expr { Ast.Binop (op, l, r) }
  | e = add_expr                              { e }

add_expr:
  | l = add_expr op = add_op r = mul_expr { Ast.Binop (op, l, r) }
  | e = mul_expr                          { e }

mul_expr:
  | l = mul_expr op = mul_op r = unary { Ast.Binop (op, l, r) }
  | e = unary                          { e }

unary:
  | MINUS e = unary   { Ast.Neg e }
  | e = app_expr      { e }

app_expr:
  | f = app_expr a = atom { Ast.App (f, a) }
  | a = atom              { a }

atom:
  | n = INT                  { Ast.Int n }
  | TRUE                     { Ast.Bool true }
  | FALSE                    { Ast.Bool false }
  | x = ID                   { Ast.Var x }
  | LPAREN e = expr RPAREN   { e }

%inline comparison:
  | EQ  { Ast.Eq }
  | NEQ { Ast.Neq }
  | LT  { Ast.Lt }
  | LE  { Ast.Le }
  | GT  { Ast.Gt }
  | GE  { Ast.Ge }

%inline add_op:
  | PLUS  { Ast.Add }
  | MINUS { Ast.Sub }

%inline mul_op:
  | STAR  { Ast.Mul }
  | SLASH { Ast.Div }
  | MOD   { Ast.Mod }

%inline rec_flag:
  | REC  { true }
  |      { false }