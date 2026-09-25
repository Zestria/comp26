%{
open Ast

let negate = function
  | Constant (Integer value) -> Constant (Integer (-value))
  | expression -> prefix "~-" expression

let positive = function
  | Constant (Integer _) as expression -> expression
  | expression -> prefix "~+" expression
%}

%token <int> INT
%token <string> IDENT
%token LET REC IN FUN IF THEN ELSE TRUE FALSE
%token UNDERSCORE LPAREN RPAREN ARROW
%token EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%token PLUS MINUS STAR SLASH AND_AND OR_OR EOF

%start <Ast.program> program
%start <Ast.expr> expression

%%

program:
  | items = list(structure_item); EOF { items }

expression:
  | body = expr; EOF { body }

structure_item:
  | LET; recursive = rec_flag; value = binding
      { let pattern, body = value in Definition (recursive, pattern, body) }

rec_flag:
  | { Nonrecursive }
  | REC { Recursive }

binding:
  | pattern = pattern; EQUAL; body = expr
      { pattern, body }
  | name = value_name; parameters = nonempty_list(parameter); EQUAL; body = expr
      { PVariable name, function_ parameters body }

expr:
  | LET; recursive = rec_flag; value = binding; IN; body = expr
      { let pattern, value = value in Let_in (recursive, pattern, value, body) }
  | FUN; parameters = nonempty_list(parameter); ARROW; body = expr
      { function_ parameters body }
  | IF; condition = expr; THEN; yes = expr; ELSE; no = expr
      { If_then_else (condition, yes, no) }
  | body = disjunction { body }

disjunction:
  | body = conjunction { body }
  | left = conjunction; OR_OR; right = disjunction { infix "||" left right }

conjunction:
  | body = comparison { body }
  | left = comparison; AND_AND; right = conjunction { infix "&&" left right }

comparison:
  | body = additive { body }
  | left = comparison; operator = comparison_operator; right = additive
      { infix operator left right }

%inline comparison_operator:
  | EQUAL { "=" }
  | NOT_EQUAL { "<>" }
  | LESS { "<" }
  | LESS_EQUAL { "<=" }
  | GREATER { ">" }
  | GREATER_EQUAL { ">=" }

additive:
  | body = multiplicative { body }
  | left = additive; PLUS; right = multiplicative { infix "+" left right }
  | left = additive; MINUS; right = multiplicative { infix "-" left right }

multiplicative:
  | body = unary { body }
  | left = multiplicative; STAR; right = unary { infix "*" left right }
  | left = multiplicative; SLASH; right = unary { infix "/" left right }

unary:
  | body = application { body }
  | MINUS; body = unary { negate body }
  | PLUS; body = unary { positive body }

application:
  | body = atom { body }
  | function_ = application; argument = atom { Application (function_, argument) }

atom:
  | value = constant { Constant value }
  | name = value_name { Variable name }
  | LPAREN; RPAREN { Constant Unit }
  | LPAREN; body = expr; RPAREN { body }

value_name:
  | name = IDENT { name }
  | LPAREN; operator = operator_name; RPAREN { operator }

%inline operator_name:
  | operator = comparison_operator { operator }
  | PLUS { "+" }
  | MINUS { "-" }
  | STAR { "*" }
  | SLASH { "/" }
  | AND_AND { "&&" }
  | OR_OR { "||" }

constant:
  | value = INT { Integer value }
  | TRUE { Boolean true }
  | FALSE { Boolean false }

pattern:
  | UNDERSCORE { PWildcard }
  | name = value_name { PVariable name }
  | LPAREN; pattern = pattern; RPAREN { pattern }

parameter:
  | pattern = pattern { pattern }
