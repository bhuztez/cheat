-module(cheat_scope).

-export([transform/2]).


transform(Forms, ModuleName) ->
    {_, Forms1, ModuleName} = lists:foldl(fun transform_form/2, {0, [], ModuleName}, Forms),
    lists:reverse(Forms1).


transform_form({fundef, {atom, F}, A, Expression}, {NFun, Funs, ModuleName}) ->
    {{Local, []}, Expression1, {[], NFun1, Funs1, ModuleName}} = transform_funbody(Expression, {[], NFun, Funs, ModuleName}),
    {NFun1, [{fundef, F, A, [], Local, Expression1}|Funs1], ModuleName}.


transform_funbody(Expression, {Scopes, NFun, Funs, ModuleName}) ->
    {Expression1, {[{Local, _, NonLocal}|Scopes1], NFun1, Funs1, ModuleName}} =
        transform_expression(Expression, {[{sets:new(), sets:new(), sets:new()}|Scopes], NFun, Funs, ModuleName}),
    {{sets:to_list(Local), sets:to_list(NonLocal)}, Expression1, {Scopes1, NFun1, Funs1, ModuleName}}.


transform_expression({arg, _}=Expr, State) ->
    {Expr, State};
transform_expression({literal, _}=Expr, State) ->
    {Expr, State};

transform_expression({varname, _, VarName} = Expr, {Scopes, NFun, Funs, ModuleName}) ->
    case lookup(VarName, Scopes) of
        not_found ->
            throw(var_not_defined);
        unsafe ->
            throw(var_not_safe);
        {ok, Scopes1} ->
            {Expr, {Scopes1, NFun, Funs, ModuleName}}
    end;

transform_expression({tuple, Expressions}, State) ->
    {Expressions1, State1} = lists:mapfoldl(fun transform_expression/2, State, Expressions),
    {{call, {atom, none, std}, {atom, none, make_tuple}, [{literal, {integer, none, length(Expressions1)}}|Expressions1]}, State1};

transform_expression({list, [], B}, State) ->
    transform_expression(B, State);

transform_expression({list, [H|T], B}, State) ->
    {H1, State1} = transform_expression(H, State),
    {T1, State2} = transform_expression({list, T, B}, State1),
    {{call, {atom, none, std}, {atom, none, cons}, [H1, T1]}, State2};

transform_expression({call, var, F, A}, State) ->
    {[F1|A1], State1} = lists:mapfoldl(fun transform_expression/2, State, [F|A]),
    {{call, F1, A1}, State1};

transform_expression({call, local, F, A}, {_, _, _, ModuleName}= State) ->
    {A1, State1} = lists:mapfoldl(fun transform_expression/2, State, A),
    {{call, {atom, none, ModuleName}, F, A1}, State1};

transform_expression({call, {atom,_,_}=M, F, A}, State) ->
    {A1, State1} = lists:mapfoldl(fun transform_expression/2, State, A),
    {{call, M, F, A1}, State1};

transform_expression({call, op, F, A}, State) ->
    {A1, State1} = lists:mapfoldl(fun transform_expression/2, State, A),
    {{call, {atom, none, std}, {atom, none, F}, A1}, State1};

transform_expression({fundef, ignore, A, Expression}, State) ->
    {{Local, NonLocal}, Expression1, {Scopes, NFun, Funs, M}}= transform_funbody(Expression, State),
    {{make_fun, {M, NFun}, NonLocal}, {Scopes, NFun+1, [{fundef, NFun, A, NonLocal, Local, Expression1}|Funs], M}};

transform_expression({'fun', local, F, A}, {_, _, _, ModuleName} = State) ->
    {{make_fun, {atom, none, ModuleName}, F, A}, State};

transform_expression({'fun', M, F, A}, State) ->
    {{make_fun, M, F, A}, State};

transform_expression({'case', Expressions, Clauses}, State) ->
    {Expressions1, State1} = lists:mapfoldl(fun transform_expression/2, State, Expressions),
    {ClausesAndLocals, {[{_, _, NonLocal}|Scopes], NFun, Funs, ModuleName}} = lists:mapfoldl(fun transform_clause/2, State1, Clauses),
    {Clauses1, Locals, Unsafes} = lists:unzip3(ClausesAndLocals),

    Unsafe = sets:union(Unsafes),
    Local = sets:union(Locals),
    Safe = sets:intersection(Locals),
    Unsafe1 = sets:subtract(Local, Safe),
    Unsafe2 = sets:union(Unsafe, Unsafe1),

    {{'case', Expressions1, Clauses1}, {[{Local, Unsafe2, NonLocal}|Scopes], NFun, Funs, ModuleName}}.


transform_matchspec({match_ignore, _}=MatchSpec, State) ->
    {MatchSpec, State};
transform_matchspec({match_literal, _}=MatchSpec, State) ->
    {MatchSpec, State};
transform_matchspec({match_fun, _}=MatchSpec, State) ->
    {MatchSpec, State};
transform_matchspec({match_tuple, MatchSpecs}, State) ->
    {MatchSpecs1, State1} = lists:mapfoldl(fun transform_matchspecs/2, State, MatchSpecs),
    {{match_tuple, MatchSpecs1}, State1};
transform_matchspec({match_list, A, B}, State) ->
    {A1, State1} = lists:mapfoldl(fun transform_matchspecs/2, State, A),
    {B1, State2} = transform_matchspecs(B, State1),
    {{match_list, A1, B1}, State2};
transform_matchspec({match_var, VarName}=MatchSpec, {[{Local, Unsafe, NonLocal}|Rest] = Scopes, NFun, Funs, ModuleName}) ->
    case lookup(VarName, Scopes) of
        unsafe ->
            throw(var_not_safe);
        not_found ->
            {{assign, VarName}, {[{sets:add_element(VarName, Local), Unsafe, NonLocal}|Rest], NFun, Funs, ModuleName}};
        {ok, Scopes1} ->
            {MatchSpec, {Scopes1, NFun, Funs, ModuleName}}
    end.


transform_matchspecs(MatchSpecs, State) ->
    lists:mapfoldl(fun transform_matchspec/2, State, MatchSpecs).


transform_clause({MatchSpecs, Guards, Expressions}, {[{Local, Unsafe, _}|_], _, _, _}= State) ->
    {MatchSpecs1, State1} = lists:mapfoldl(fun transform_matchspecs/2, State, MatchSpecs),
    {Guards1, State2} = lists:mapfoldl(fun transform_expression/2, State1, Guards),
    {Expressions1, {[{Local1, Unsafe1, NonLocal}|Scopes], NFun, Funs, ModuleName}} = lists:mapfoldl(fun transform_expression/2, State2, Expressions),

    {{{MatchSpecs1, Guards1, Expressions1}, Local1, Unsafe1},
     {[{Local, Unsafe, NonLocal}|Scopes], NFun, Funs, ModuleName}}.


lookup(_, []) ->
    not_found;
lookup(VarName, [{Local, Unsafe, NonLocal}|Rest] = Scopes) ->
    case sets:is_element(VarName, Unsafe) of
        true ->
            unsafe;
        false ->
            case sets:is_element(VarName, Local) of
                true ->
                    {ok, Scopes};
                false ->
                    case lookup(VarName, Rest) of
                        not_found ->
                            not_found;
                        unsafe ->
                            unsafe;
                        {ok, Rest1} ->
                            {ok, [{sets:add_element(VarName, Local), Unsafe, sets:add_element(VarName, NonLocal)}|Rest1]}
                    end
            end
    end.
