Nonterminals
forms form clause_list clause clause_body
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

clause_list -> clause:
    ['$1'].

clause -> varname clause_body:
    clause('$1', '$2').

clause -> atom clause_body:
    clause('$1', '$2').

clause -> clause_body:
    clause(ignore, '$1').

clause_body -> '(' ')' '->' expression_list:
    {body, line_of('$1'), {[], [], '$4'}}.

clause_body -> '(' expression_list ')' '->'  expression_list:
    {body, line_of('$1'), {'$2', [], '$5'}}.

clause_body -> '(' expression_list ')' when expression_list '->'  expression_list:
    {body, line_of('$1'), {'$2', '$5', '$7'}}.

expression_list -> expression_list ',' expression:
    '$1' ++ ['$3'].

expression_list -> expression:
    ['$1'].

expression -> '(' expression ')':
    '$2'.

expression -> not expression:
    op1('not', '$1', '$2').


expression -> uminus:
    '$1'.

uminus -> '-' expression:
    op1(neg, '$1', '$2').

expression -> expression '**' expression:
    op2(pow, '$1', '$2', '$3').

expression -> expression '*' expression:
    op2(mul, '$1', '$2', '$3').

expression -> expression '/' expression:
    op2('div', '$1', '$2', '$3').

expression -> expression '%' expression:
    op2('mod', '$1', '$2', '$3').

expression -> expression '+' expression:
    op2(plus, '$1', '$2', '$3').

expression -> expression '-' expression:
    op2(minus, '$1', '$2', '$3').

expression -> expression '==' expression:
    op2(eq, '$1', '$2', '$3').

expression -> expression '/=' expression:
    op2(ne, '$1', '$2', '$3').

expression -> expression '<' expression:
    op2(lt, '$1', '$2', '$3').

expression -> expression '=<' expression:
    op2(le, '$1', '$2', '$3').

expression -> expression '>=' expression:
    op2(ge, '$1', '$2', '$3').

expression -> expression '>' expression:
    op2(gt, '$1', '$2', '$3').

expression -> expression '=:=' expression:
    op2(exact, '$1', '$2', '$3').

expression -> expression '=/=' expression:
    op2(notexact, '$1', '$2', '$3').

expression -> expression and expression:
    op2('and', '$1', '$2', '$3').

expression -> expression or expression:
    op2('or', '$1', '$2', '$3').

expression -> expression '=' expression:
    {match, line_of('$2'), {'$1', '$3'}}.

expression -> atom ':' atom '(' ')':
    {call, line_of('$3'), {'$1', '$3', []}}.

expression -> atom ':' atom '(' expression_list ')':
    {call, line_of('$3'), {'$1', '$3', '$5'}}.

expression -> atom '(' ')':
    {call, line_of('$1'), {local, '$1', []}}.

expression -> atom '(' expression_list ')':
    {call, line_of('$1'), {local, '$1', '$3'}}.

expression -> varname '(' ')':
    {call, line_of('$1'), {var, '$1', []}}.

expression -> varname '(' expression_list ')':
    {call, line_of('$1'), {var, '$1', '$3'}}.

expression -> case expression of case_clause_list end:
    {'case', line_of('$1'), {'$2', '$4'}}.

expression -> fun atom '/' integer:
    {'fun', line_of('$1'), {local, '$2', '$4'}}.

expression -> fun atom ':' atom '/' integer:
    {'fun', line_of('$1'), {'$2', '$4', '$6'}}.

expression -> fun clause_list end:
    {'fun', line_of('$1'), '$2'}.

expression -> '{' '}':
    {tuple, line_of('$1'), []}.

expression -> '{' expression_list '}':
    {tuple, line_of('$1'), '$2'}.

expression -> '[' ']':
    {literal, line_of('$1'), nil}.

expression -> '[' expression_list ']':
    {list, line_of('$1'), {'$2', {literal, line_of('$3'), nil}}}.

expression -> '[' expression_list '|' expression ']':
    {list, line_of('$1'), {'$2', '$4'}}.

expression -> '[' expression '||' lc_expression_list ']':
    {list_comprehension, line_of('$1'), {'$2', '$4'}}.

expression -> varname:
    '$1'.

expression -> atom:
    literal('$1').

expression -> integer:
    literal('$1').

expression -> string:
    literal('$1').

expression -> ignore:
    '$1'.

lc_expression_list -> lc_expression_list ',' lc_expression:
    '$1' ++ ['$3'].

lc_expression_list -> lc_expression:
    ['$1'].

lc_expression -> expression:
    '$1'.

lc_expression -> expression '<-' expression:
    {generate, line_of('$1'), {'$1', '$3'}}.

case_clause_list -> case_clause_list ';' case_clause:
    '$1' ++ ['$3'].

case_clause_list -> case_clause:
    ['$1'].

case_clause -> expression '->' expression_list:
    {case_clause, line_of('$1'), {'$1', [], '$3'}}.

case_clause -> expression when expression_list '->' expression_list:
    {case_clause, line_of('$1'), {'$1', '$3', '$5'}}.

Erlang code.

literal(T) ->
    {literal, line_of(T), T}.

line_of(Token) ->
    element(2, Token).

op1(Op, T1, T2) ->
    {call,
     line_of(T1),
     {{atom, line_of(T1), std},
      {atom, line_of(T1), Op},
      [T2]}}.

op2(Op, T1, T2, T3) ->
    {call,
     line_of(T2),
     {{atom, line_of(T2), std},
      {atom, line_of(T2), Op},
      [T1, T3]}}.

clause(ignore, {body, Line, {M,G,E}}) ->
    {clause, Line, {ignore, M, G, E}};
clause(Name, {body, _, {M,G,E}}) ->
    {clause, line_of(Name), {Name, M, G, E}}.
