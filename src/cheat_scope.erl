-module(cheat_scope).

-export([transform/2]).

transform(Forms, #{module := Module}) ->
    {_, Forms1, Module} =
        lists:foldl(
          fun transform_form/2,
          {0, [], Module},
          Forms),
    lists:reverse(Forms1).

transform_form({fundef, Line, {F, A, Expr}}, {NFun, Funs, Module}) ->
    {Local, _NonLocal, Expr1, [], NFun1, Funs1} =
        transform_funbody(Expr, {[], NFun, Funs, Module}),
    {NFun1, [{fundef, Line, {Module, F, A}, [], Local, Expr1}|Funs1], Module}.

transform_funbody(Expr, {Scopes, NFun, Funs, Module}) ->
    {Expr1, {[{Local, _, NonLocal}|Scopes1], NFun1, Funs1, Module}} =
        transform_expr(
          Expr,
          {[{sets:new(),sets:new(),sets:new()}|Scopes],
           NFun, Funs, Module}),
    {sets:to_list(Local), sets:to_list(NonLocal),
     Expr1, Scopes1, NFun1, Funs1}.

transform_expr({arg, _}=Expr, State) ->
    {Expr, State};
transform_expr({literal, _, _}=Expr, State) ->
    {Expr, State};
transform_expr({make_fun, _, _}=Expr, State) ->
    {Expr, State};
transform_expr({varname,_,VarName}=Expr, {Scopes,NFun,Funs,Module}) ->
    case lookup(VarName, Scopes) of
        not_found ->
            throw(var_not_defined);
        unsafe ->
            throw(var_not_safe);
        {ok, Scopes1} ->
            {Expr, {Scopes1, NFun, Funs, Module}}
    end;
transform_expr({call, Line, {F, A}}, State) ->
    {[F1|A1], State1} =
        lists:mapfoldl(fun transform_expr/2, State, [F|A]),
    {{call, Line, {F1, A1}}, State1};
transform_expr({fundef, Line, {ignore,A,Expr}}, {_,_,_,Module}=State) ->
    {Local, NonLocal, Expr1, Scopes, NFun, Funs} =
        transform_funbody(Expr, State),
    {{make_fun, Line, {Module, NFun, A, NonLocal}},
     {Scopes, NFun+1,
      [{fundef, Line, {Module, NFun, A}, NonLocal, Local, Expr1}|Funs], Module}};

transform_expr({'case', Line, {Exprs, Clauses}}, State) ->
    {Exprs1, State1} =
        lists:mapfoldl(fun transform_expr/2, State, Exprs),
    {ClausesAndLocals,{[{_,_,NonLocal}|Scopes],NFun,Funs,Module}} =
        lists:mapfoldl(fun transform_clause/2, State1, Clauses),
    {Clauses1, Locals, Unsafes} = lists:unzip3(ClausesAndLocals),
    Unsafe = sets:union(Unsafes),
    Local = sets:union(Locals),
    Safe = sets:intersection(Locals),
    Unsafe1 = sets:subtract(Local, Safe),
    Unsafe2 = sets:union(Unsafe, Unsafe1),
    {{'case', Line,
      {Exprs1, Clauses1}},
      {[{Local, Unsafe2, NonLocal}|Scopes], NFun, Funs, Module}}.


transform_ms({match_ignore, _, _}=MS, State) ->
    {MS, State};
transform_ms({match_literal, _, _}=MS, State) ->
    {MS, State};
transform_ms({match_fun, _, _}=MS, State) ->
    {MS, State};
transform_ms({match_tuple, Line, MSs}, State) ->
    {MSs1, State1} = lists:mapfoldl(fun transform_mss/2, State, MSs),
    {{match_tuple, Line, MSs1}, State1};
transform_ms({match_list, Line, {A, B}}, State) ->
    {A1, State1} = lists:mapfoldl(fun transform_mss/2, State, A),
    {B1, State2} = transform_mss(B, State1),
    {{match_list, Line, {A1, B1}}, State2};
transform_ms(
  {match_var, Line, VarName}=MS,
  {[{Local,Unsafe,NonLocal}|Rest]=Scopes,NFun,Funs,Module}) ->
    case lookup(VarName, Scopes) of
        unsafe ->
            throw(var_not_safe);
        not_found ->
            {{assign,Line,VarName},
             {[{sets:add_element(VarName,Local),Unsafe,NonLocal}|Rest],
              NFun,Funs,Module}};
        {ok, Scopes1} ->
            {MS, {Scopes1,NFun,Funs,Module}}
    end.


transform_mss(MSs, State) ->
    lists:mapfoldl(fun transform_ms/2, State, MSs).


transform_clause(
  {MSs,Guards,Exprs},
  {[{Local,Unsafe,_}|_],_,_,_}=State) ->
    {MSs1, State1} =
        lists:mapfoldl(fun transform_mss/2, State, MSs),
    {Guards1, State2} =
        lists:mapfoldl(fun transform_expr/2, State1, Guards),
    {Exprs1, {[{Local1, Unsafe1, NonLocal}|Scopes],NFun,Funs,Module}} =
        lists:mapfoldl(fun transform_expr/2, State2, Exprs),
    {{{MSs1, Guards1, Exprs1}, Local1, Unsafe1},
     {[{Local, Unsafe, NonLocal}|Scopes], NFun, Funs, Module}}.


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
