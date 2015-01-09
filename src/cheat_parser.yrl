Nonterminals
forms form clause_list clause
expression_list expression lc_expression_list lc_expression
case_clause_list case_clause uminus.

Terminals
varname atom string integer ignore
case of end when fun
'(' ')' '[' ']' '|' '||' '{' '}' ',' ';' '.' ':' '<-' '->' '='
'**'
'*' '/' '%'
'+' '-'
'==' '/=' '<' '=<' '>=' '>' '=:=' '=/='
and or
not.

Rootsymbol forms.

Unary    900 uminus not.
Left     800 '**'.
Left     700 '*' '/' '%'.
Left     600 '+' '-'.
Nonassoc 500 '==' '/=' '<' '=<' '>=' '>' '=:=' '=/='.
Left     400 and.
Left     300 or.
Right    100 '='.


forms -> forms form:
    '$1' ++ ['$2'].

forms -> '$empty':
    [].

form -> clause_list '.':
    {'fun', '$1'}.

clause_list -> clause_list ';' clause:
    '$1' ++ ['$3'].

clause_list -> varname clause:
    [{clause, '$1', '$2'}].

clause_list -> atom clause:
    [{clause, '$1', '$2'}].

clause_list -> clause:
    [{clause, ignore, '$1'}].

clause -> '(' ')' '->' expression_list:
    {[], [], '$4'}.

clause -> '(' expression_list ')' '->'  expression_list:
    {'$2', [], '$5'}.

clause -> '(' expression_list ')' when expression_list '->'  expression_list:
    {'$2', '$5', '$7'}.

expression_list -> expression_list ',' expression:
    '$1' ++ ['$3'].

expression_list -> expression:
    ['$1'].

expression -> '(' expression ')':
    '$2'.

expression -> not expression:
    {call, op, 'not', ['$2']}.

expression -> uminus:
    '$1'.

uminus -> '-' expression:
    {call, op, neg, ['$2']}.

expression -> expression '**' expression:
    {call, op, pow, ['$1', '$3']}.

expression -> expression '*' expression:
    {call, op, mul, ['$1', '$3']}.

expression -> expression '/' expression:
    {call, op, 'div', ['$1', '$3']}.

expression -> expression '%' expression:
    {call, op, mod, ['$1', '$3']}.

expression -> expression '+' expression:
    {call, op, plus, ['$1', '$3']}.

expression -> expression '-' expression:
    {call, op, minus, ['$1', '$3']}.

expression -> expression '==' expression:
    {call, op, eq, ['$1', '$3']}.

expression -> expression '/=' expression:
    {call, op, ne, ['$1', '$3']}.

expression -> expression '<' expression:
    {call, op, lt, ['$1', '$3']}.

expression -> expression '=<' expression:
    {call, op, le, ['$1', '$3']}.

expression -> expression '>=' expression:
    {call, op, ge, ['$1', '$3']}.

expression -> expression '>' expression:
    {call, op, gt, ['$1', '$3']}.

expression -> expression '=:=' expression:
    {call, op, exact, ['$1', '$3']}.

expression -> expression '=/=' expression:
    {call, op, notexact, ['$1', '$3']}.

expression -> expression and expression:
    {call, op, 'and', ['$1', '$3']}.

expression -> expression or expression:
    {call, op, 'or', ['$1', '$3']}.

expression -> expression '=' expression:
    {match, '$1', '$3'}.

expression -> atom ':' atom '(' ')':
    {call, '$1', '$3', []}.

expression -> atom ':' atom '(' expression_list ')':
    {call, '$1', '$3', '$5'}.

expression -> atom '(' ')':
    {call, local, '$1', []}.

expression -> atom '(' expression_list ')':
    {call, local, '$1', '$3'}.

expression -> varname '(' ')':
    {call, var, '$1', []}.

expression -> varname '(' expression_list ')':
    {call, var, '$1', '$3'}.

expression -> case expression of case_clause_list end:
    {'case', '$2', '$4'}.

expression -> fun atom '/' integer:
    {'fun', local, '$2', '$4'}.

expression -> fun atom ':' atom '/' integer:
    {'fun', '$2', '$4', '$6'}.

expression -> fun clause_list end:
    {'fun', '$2'}.

expression -> '{' '}':
    {tuple, []}.

expression -> '{' expression_list '}':
    {tuple, '$2'}.

expression -> '[' ']':
    {list, [], {literal, nil}}.

expression -> '[' expression_list ']':
    {list, '$2', {literal, nil}}.

expression -> '[' expression_list '|' expression ']':
    {list, '$2', '$4'}.

expression -> '[' expression '||' lc_expression_list ']':
    {list_comprehension, '$2', '$4'}.

expression -> varname:
    '$1'.

expression -> atom:
    {literal, '$1'}.

expression -> integer:
    {literal, '$1'}.

expression -> string:
    {literal, '$1'}.

expression -> ignore:
    '$1'.

lc_expression_list -> lc_expression_list ',' lc_expression:
    '$1' ++ ['$3'].

lc_expression_list -> lc_expression:
    ['$1'].

lc_expression -> expression:
    '$1'.

lc_expression -> expression '<-' expression:
    {generate, '$1', '$3'}.

case_clause_list -> case_clause_list ';' case_clause:
    '$1' ++ ['$3'].

case_clause_list -> case_clause:
    ['$1'].

case_clause -> expression '->' expression_list:
    {'$1', [], '$3'}.

case_clause -> expression when expression_list '->' expression_list:
    {'$1', '$3', '$5'}.

Erlang code.
