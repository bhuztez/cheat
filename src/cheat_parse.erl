-module(cheat_parse).

-export([file/1, string/1]).

file(Filename) ->
    {ok, Content} = file:read_file(Filename),
    string(Content).

string(Source) ->
    {ok, Tokens, _} = cheat_lexer:string(binary_to_list(Source)),
    {ok, Forms} = cheat_parser:parse(Tokens),
    [transform_form(Form) || Form <- Forms].

transform_form({'fun', _} = Fun) ->
    {fundef, {atom, _}, _, _} = transform_fun(Fun).

all_same([{F,A}|T]) ->
    all_same(T, {F,A}).

all_same([], X) ->
    {true, X};
all_same([X|T], X) ->
    all_same(T, X);
all_same(_, _) ->
    false.

transform_fun({'fun', Clauses}) ->
    Clauses1 = [transform_clause(Clause) || Clause <- Clauses],
    {true, {F,A}} = all_same([{X,Y} || {clause, X, Y, _, _, _} <- Clauses1]),
    {fundef, F, A,
     {'case', [{arg, N}|| N <- lists:seq(0,A-1)],
      [{M,G,E} || {clause, _, _, M, G, E} <- Clauses1]}}.

clause_name(ignore) ->
    ignore;
clause_name({varname, _, Name}) ->
    {varname, Name};
clause_name({atom, _, Atom}) ->
    {atom, Atom}.

transform_clause({clause, Name, {Matchspecs, Guards, Expressions}}) ->
    {clause, clause_name(Name), length(Matchspecs),
     [transform_matchspec(Matchspec) || Matchspec <- Matchspecs],
     [transform_guard(Guard) || Guard <- Guards],
     [transform_expression(Expression) || Expression <- Expressions]}.

transform_matchspec({'case', _, _}) ->
    throw("illegal pattern");
transform_matchspec({call, _, _, _}) ->
    throw("illegal pattern");
transform_matchspec({list_comprehension, _, _}) ->
    throw("illegal pattern");
transform_matchspec({'fun', _}) ->
    throw("illegal pattern");
transform_matchspec({match, A, B}) ->
    transform_matchspec(A) ++ transform_matchspec(B);
transform_matchspec({tuple, Matchspecs}) ->
    [{match_tuple,
      [transform_matchspec(Matchspec) || Matchspec <- Matchspecs]}];
transform_matchspec({list, A, B}) ->
    [{match_list,
      [transform_matchspec(Matchspec) || Matchspec <- A],
      transform_matchspec(B)}];
transform_matchspec({varname, _, X}) ->
    [{match_var, X}];
transform_matchspec({ignore, _, X}) ->
    [{match_ignore, X}];
transform_matchspec({literal, X}) ->
    [{match_literal, X}];
transform_matchspec({'fun', M,F,A}) ->
    [{match_fun, M, F, A}].

transform_guard({'case', _, _}) ->
    throw("illegal guard");
transform_guard({match, _, _}) ->
    throw("illegal guard");
transform_guard({list_comprehension, _, _}) ->
    throw("illegal guard");
transform_guard({ignore, _, _}) ->
    throw("illegal guard");
transform_guard({'fun', _}) ->
    throw("illegal guard");
transform_guard({tuple, Expressions}) ->
    {tuple, [transform_guard(Expression) || Expression <- Expressions ]};
transform_guard({list, A, B}) ->
    {list,
     [transform_guard(Expression) || Expression <- A],
     transform_guard(B)};
transform_guard({varname, _, _}=X) ->
    X;
transform_guard({literal, _} = X) ->
    X;
transform_guard({'fun',_,_,_}=X) ->
    X;
transform_guard({call, M, F, A}) ->
    {call, M, F, [transform_guard(Expression)||Expression<-A]}.

transform_expression({ignore, _, _}) ->
    throw("illegal expression");
transform_expression({list_comprehension, _, _}) ->
    throw("TODO");
transform_expression({varname, _, _}=X) ->
    X;
transform_expression({literal, _} = X) ->
    X;
transform_expression({'fun',_,_,_}=X) ->
    X;
transform_expression({tuple, Expressions}) ->
    {tuple, [transform_expression(Expression) || Expression <- Expressions ]};
transform_expression({list, A, B}) ->
    {list,
     [transform_expression(Expression) || Expression <- A],
     transform_expression(B)};
transform_expression({call, M, F, A}) ->
    {call, M, F, [transform_expression(Expression)||Expression<-A]};
transform_expression({'case', Expr, Clauses}) ->
    {'case',
     [transform_expression(Expr)],
     [transform_case_clause(Clause) || Clause <- Clauses]};
transform_expression({match, A, B}) ->
    Matchspec = transform_matchspec(A),
    case transform_expression(B) of
        {'case', [Expression], [{[Matchspec1], [],[]}]} ->
            {'case', [Expression], [{[Matchspec++Matchspec1], [], []}]};
        Expression ->
            {'case', [Expression], [{[Matchspec], [], []}]}
    end;
transform_expression({'fun', _} = Fun) ->
    case transform_fun(Fun) of
        {'fun', {atom, _}, _, _} ->
            throw("invalid fun");
        Fun1 ->
            Fun1
    end.

transform_case_clause({Matchspec, Guards, Expressions}) ->
    {[transform_matchspec(Matchspec)],
     [transform_guard(Guard) || Guard <- Guards],
     [transform_expression(Expression) || Expression <- Expressions]}.
