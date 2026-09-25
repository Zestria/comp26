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
%token LET REC AND IN FUN FUNCTION IF THEN ELSE MATCH WITH TRUE FALSE
%token UNDERSCORE LPAREN RPAREN LBRACKET RBRACKET COMMA SEMICOLON BAR ARROW
%token EQUAL NOT_EQUAL LESS LESS_EQUAL GREATER GREATER_EQUAL
%token PLUS MINUS TILDE_PLUS TILDE_MINUS STAR SLASH AND_AND OR_OR CONS EOF

/* A following case belongs to the innermost unparenthesized match/function. */
%nonassoc below_bar
%nonassoc BAR

%start <Ast.program> program
%start <Ast.expr> expression

%%

program:
  | items = list(structure_item); EOF { items }

expression:
  | body = expr; EOF { body }

structure_item:
  | LET; recursive = rec_flag; bindings = bindings; option(terminator)
      { let first, rest = bindings in Value (recursive, first, rest) }

terminator:
  | SEMICOLON; SEMICOLON { () }

rec_flag:
  | { Nonrecursive }
  | REC { Recursive }

bindings:
  | first = binding; rest = list(preceded(AND, binding)) { first, rest }

binding:
  | pattern = pattern; EQUAL; body = expr
      { { pattern; expression = body } }
  | name = value_name; parameters = nonempty_list(parameter); EQUAL; body = expr
      { { pattern = PVariable name; expression = function_ parameters body } }

expr:
  | LET; recursive = rec_flag; bindings = bindings; IN; body = expr
      { let first, rest = bindings in Let_in (recursive, first, rest, body) }
  | FUN; parameters = nonempty_list(parameter); ARROW; body = expr
      { function_ parameters body }
  | IF; condition = expr; THEN; yes = expr; ELSE; no = expr
      { If_then_else (condition, yes, no) }
  | MATCH; subject = expr; WITH; cases = cases
      { let first, rest = cases in Match (subject, first, rest) }
  | FUNCTION; cases = cases
      { let first, rest = cases in Function (first, rest) }
  | body = tuple_expr { body }

cases:
  | option(BAR); cases = case_list { cases }

case_list:
  | first = case %prec below_bar { first, [] }
  | first = case; BAR; rest = case_list
      { let next, tail = rest in first, next :: tail }

case:
  | pattern = pattern; ARROW; body = expr
      { { case_pattern = pattern; case_expression = body } }

tuple_expr:
  | body = disjunction { body }
  | first = disjunction; COMMA; second = disjunction;
    rest = list(preceded(COMMA, disjunction))
      { Tuple (first, second, rest) }

disjunction:
  | body = conjunction { body }
  | left = conjunction; OR_OR; right = disjunction { infix "||" left right }

conjunction:
  | body = comparison { body }
  | left = comparison; AND_AND; right = conjunction { infix "&&" left right }

comparison:
  | body = cons_expr { body }
  | left = comparison; operator = comparison_operator; right = cons_expr
      { infix operator left right }

%inline comparison_operator:
  | EQUAL { "=" }
  | NOT_EQUAL { "<>" }
  | LESS { "<" }
  | LESS_EQUAL { "<=" }
  | GREATER { ">" }
  | GREATER_EQUAL { ">=" }

cons_expr:
  | body = additive { body }
  | head = additive; CONS; tail = cons_expr { infix "::" head tail }

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
  | body = prefix_expr { body }
  | function_ = application; argument = prefix_expr { Application (function_, argument) }

prefix_expr:
  | body = atom { body }
  | TILDE_MINUS; body = prefix_expr { prefix "~-" body }
  | TILDE_PLUS; body = prefix_expr { prefix "~+" body }

atom:
  | value = constant { Constant value }
  | name = value_name { Variable name }
  | LPAREN; RPAREN { Constant Unit }
  | LPAREN; body = expr; RPAREN { body }
  /* Sequences are absent from Ast: control expressions in lists need parentheses. */
  | LBRACKET; elements = list_elements(tuple_expr); RBRACKET { List elements }

value_name:
  | name = IDENT { name }
  | LPAREN; operator = operator_name; RPAREN { operator }

%inline operator_name:
  | operator = comparison_operator { operator }
  | PLUS { "+" }
  | MINUS { "-" }
  | TILDE_PLUS { "~+" }
  | TILDE_MINUS { "~-" }
  | STAR { "*" }
  | SLASH { "/" }
  | AND_AND { "&&" }
  | OR_OR { "||" }
  | CONS { "::" }

constant:
  | value = INT { Integer value }
  | TRUE { Boolean true }
  | FALSE { Boolean false }

pattern:
  | pattern = cons_pattern { pattern }
  | first = cons_pattern; COMMA; second = cons_pattern;
    rest = list(preceded(COMMA, cons_pattern))
      { PTuple (first, second, rest) }

cons_pattern:
  | pattern = signed_pattern { pattern }
  | head = signed_pattern; CONS; tail = cons_pattern { PCons (head, tail) }

signed_pattern:
  | pattern = parameter { pattern }
  | MINUS; value = INT { PConstant (Integer (-value)) }
  | PLUS; value = INT { PConstant (Integer value) }

parameter:
  | UNDERSCORE { PWildcard }
  | name = value_name { PVariable name }
  | value = constant { PConstant value }
  | LPAREN; RPAREN { PConstant Unit }
  | LPAREN; pattern = pattern; RPAREN { pattern }
  | LBRACKET; elements = list_elements(pattern); RBRACKET { PList elements }

list_elements(element):
  | { [] }
  | head = element; tail = list_tail(element) { head :: tail }

list_tail(element):
  | { [] }
  | SEMICOLON; tail = list_elements(element) { tail }
