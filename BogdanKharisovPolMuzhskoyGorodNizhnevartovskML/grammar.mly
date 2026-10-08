/* SPDX-License-Identifier: LGPL-3.0-or-later */

%{
open Ast
%}

%token <Name.t> IDENT PREFIX HASHOP INFIX0 INFIX1 INFIX2 INFIX3 INFIX4
%token <Name.t> MINUS EQUAL AND OR ASSIGN
%token <Integer_literal.t> INT
%token TRUE FALSE LET REC IN FUN ARROW IF THEN ELSE
%token UNDERSCORE LPAREN RPAREN COMMA SEMISEMI EOF

%nonassoc IN
%nonassoc THEN
%nonassoc ELSE
%right ASSIGN
%nonassoc below_COMMA
%left COMMA
%right OR
%right AND
%left INFIX0 EQUAL
%right INFIX1
%left INFIX2 MINUS
%left INFIX3
%right INFIX4
%nonassoc unary_minus
%nonassoc constructor_atom
%left HASHOP
%nonassoc IDENT INT TRUE FALSE LPAREN PREFIX

%start <Ast.expression> expression
%start <Ast.program> program

%%

expression:
  e = expr EOF { e }

program:
  initial = ioption(expr) rest = structure_tail EOF
    { Option.fold ~none:rest ~some:(fun e -> Evaluation e :: rest) initial }

structure_tail:
  | { [] }
  | b = binding rest = structure_tail { Definition b :: rest }
  | SEMISEMI initial = ioption(expr) rest = structure_tail
      { Option.fold ~none:rest ~some:(fun e -> Evaluation e :: rest) initial }

binding:
  LET recursion = recursion body = binding_body
    { let pattern, expression = body in { recursion; pattern; expression } }

recursion:
  | { Nonrecursive }
  | REC { Recursive }

binding_body:
  | p = pattern EQUAL e = expr { p, e }
  | name = value_name p = simple_pattern ps = list(simple_pattern) EQUAL body = expr
      { PVar name, Fun { parameter = p; parameters = ps; body } }

pattern:
  | p = simple_pattern { p }
  | first = simple_pattern COMMA second = simple_pattern
    rest = list(preceded(COMMA, simple_pattern)) { PTuple (first, second, rest) }

simple_pattern:
  | name = value_name { PVar name }
  | UNDERSCORE { PWildcard }
  | LPAREN RPAREN { PUnit }
  | LPAREN p = pattern RPAREN { p }

expr:
  | e = simple_expr { e }
  | f = simple_expr first = simple_expr rest = list(simple_expr)
      { Apply (f, first, rest) }
  | c = constructor e = simple_expr { Apply (Constant c, e, []) }
  | elements = tuple_elements %prec below_COMMA
      { let first, second, rest = elements in Tuple (first, second, List.rev rest) }
  | left = expr op = infix right = expr { Ast_helper.binary op left right }
  | MINUS e = expr %prec unary_minus { Ast_helper.negate e }
  | b = binding IN body = expr { Let (b, body) }
  | FUN parameter = simple_pattern parameters = list(simple_pattern) ARROW body = expr
      %prec IN
      { Fun { parameter; parameters; body } }
  | IF condition = expr THEN yes = expr ELSE no = expr { If (condition, yes, Some no) }
  | IF condition = expr THEN yes = expr { If (condition, yes, None) }

tuple_elements:
  | first = expr COMMA second = expr { first, second, [] }
  | elements = tuple_elements COMMA next = expr
      { let first, second, rest = elements in first, second, next :: rest }

simple_expr:
  | name = value_name { Var name }
  | literal = INT { Constant (Int literal) }
  | c = constructor %prec constructor_atom { Constant c }
  | LPAREN e = expr RPAREN { e }
  | op = PREFIX e = simple_expr { Apply (Var op, e, []) }
  | left = simple_expr op = HASHOP right = simple_expr { Ast_helper.binary op left right }

constructor:
  | TRUE { Bool true }
  | FALSE { Bool false }
  | LPAREN RPAREN { Unit }

value_name:
  | name = IDENT { name }
  | LPAREN name = operator RPAREN { name }

%inline infix:
  | name = INFIX0 { name }
  | name = EQUAL { name }
  | name = INFIX1 { name }
  | name = INFIX2 { name }
  | name = MINUS { name }
  | name = INFIX3 { name }
  | name = INFIX4 { name }
  | name = AND { name }
  | name = OR { name }
  | name = ASSIGN { name }

operator:
  | name = infix { name }
  | name = PREFIX { name }
  | name = HASHOP { name }
