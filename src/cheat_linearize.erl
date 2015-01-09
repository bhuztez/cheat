-module(cheat_linearize).

-export([transform/1]).


transform(Forms) ->
    [transform_form(Form) || Form <- Forms].


transform_form({fundef, F, A, NonLocal, Local, Expression}) ->
    NNL = length(NonLocal),
    Local1 =
        sets:to_list(
          sets:subtract(
            sets:from_list(Local),
            sets:from_list(NonLocal))),
    NL = length(Local1),

    VarNameMap =
        dict:from_list(
          lists:zip(NonLocal, lists:seq(0, NNL-1))
          ++ [{{arg, N}, NNL+N} || N <- lists:seq(0, A-1)]
          ++ lists:zip(Local1, lists:seq(NNL+A, NNL+A+NL-1))),

    {{VarNum, Insts}, {_, _NVar, VarNameMap}} = transform_expression(Expression, {0, NNL+A+NL, VarNameMap}),
    Insts1 = Insts ++ [{return, VarNum}],
    {fundef, F, A, NNL, Insts1}.


transform_expression({arg, _}=Arg, {_, _, VarNameMap}=State) ->
    {{dict:fetch(Arg, VarNameMap), []}, State};
transform_expression({varname, _, VarName}, {_, _, VarNameMap}=State) ->
    {{dict:fetch(VarName, VarNameMap), []}, State};
transform_expression({literal, Literal}, {NLabel, NVar, VarNameMap}) ->
    {{NVar, [{literal, Literal, NVar}]}, {NLabel, NVar+1, VarNameMap}};

transform_expression({make_fun, {atom, _, M}, {atom, _, F}, {integer, _, A}}, {NLabel, NVar, VarNameMap}) ->
    {{NVar, [{literal, {funref, none, {M, F, A}}, NVar}]}, {NLabel, NVar+1, VarNameMap}};

transform_expression({make_fun, N, VarNames}, {NLabel, NVar, VarNameMap}) ->
    VarNums = [ dict:fetch(VarName, VarNameMap) || VarName <- VarNames ],
    {{NVar+2,
      [{literal, {funref, none, {std, make_fun, length(VarNums)+1}}, NVar},
       {literal, {funref, none, N}, NVar+1},
       {call, NVar, [NVar+1|VarNums], NVar+2}]},
     {NLabel, NVar+3, VarNameMap}};

transform_expression({call, F, Expressions}, State) ->
    {VarNums, Insts, {NLabel, NVar, VarNameMap}} = transform_expressions(Expressions, State),
    FNum = dict:fetch(F, VarNameMap),
    Insts1 = [[{call, FNum, VarNums, NVar}]],
    Insts2 = lists:append(Insts ++ Insts1),
    {{NVar, Insts2}, {NLabel, NVar+1, VarNameMap}};

transform_expression({call, {atom,_,M}, {atom, _, F}, Expressions}, State) ->
    {VarNums, Insts, {NLabel, NVar, VarNameMap}} = transform_expressions(Expressions, State),
    Insts1 =
        [[ {literal, {funref, none, {M, F, length(VarNums)}}, NVar},
           {call, NVar, VarNums, NVar+1} ]],
    Insts2 = lists:append(Insts ++ Insts1),
    {{NVar+1, Insts2}, {NLabel, NVar+2, VarNameMap}};

transform_expression({'case', Expressions, Clauses}, State) ->
    {VarNums, Insts, {NLabel, NVar, VarNameMap}} = transform_expressions(Expressions, State),
    {Insts1, {OutLabel, OutVar, VarNums, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(fun transform_clause/2, {NLabel, NVar, VarNums, NLabel+1, NVar+1, VarNameMap}, Clauses),

    Insts2 =
        lists:append(
          Insts ++ Insts1
          ++ [[ badmatch, {label, OutLabel} ]]),

    {{OutVar, Insts2}, {NLabel1, NVar1, VarNameMap}}.

transform_expressions(Expressions, State) ->
    {VarNumsAndInsts, State1} = lists:mapfoldl(fun transform_expression/2, State, Expressions),
    {VarNums, Insts} = lists:unzip(VarNumsAndInsts),
    {VarNums, Insts, State1}.


transform_guard(Guard, {EndLabel, NLabel, NVar, VarNameMap}) ->
    {{VarNum, Insts}, {NLabel1, NVar1, VarNameMap}} =
        transform_expression(Guard, {NLabel, NVar, VarNameMap}),
    Insts1 =
        lists:append(Insts ++ [[{branch, VarNum, EndLabel}]]),
    {Insts1, {EndLabel, NLabel1, NVar1, VarNameMap}}.


transform_matchspec({assign, VarName}, {VarNum, _, _, _, VarNameMap}=State) ->
    VarNum1 = dict:fetch(VarName, VarNameMap),
    {[{move, VarNum, VarNum1}], State};
transform_matchspec({match_var, VarName}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    VarNum1 = dict:fetch(VarName, VarNameMap),
    Insts =
        [ {literal, {funref, none, {std, match, 2}}, NVar},
          {call, NVar, [VarNum, VarNum1], NVar+1},
          {branch, NVar+1, EndLabel} ],
    {Insts, {VarNum, EndLabel, NLabel, NVar+2, VarNameMap}};
transform_matchspec({match_literal, Literal}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [{literal, Literal, NVar},
         {literal, {funref, none, {std, match, 2}}, NVar+1},
         {call, NVar+1, [VarNum, NVar], NVar+2},
         {branch, NVar+2, EndLabel}],
    {Insts, {VarNum, EndLabel, NLabel, NVar+2, VarNameMap}};
transform_matchspec({match_tuple, MatchSpecs}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    NM = length(MatchSpecs),
    Insts =
        [[{literal, {funref, none, {std, is_tuple, 2}}, NVar},
          {call, NVar, [VarNum, NM], NVar+1},
          {branch, NVar+1, EndLabel}]],

    {Insts1, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(
          fun transform_match_tuple_element/2,
          {VarNum, EndLabel, NLabel, NVar+2, VarNameMap},
          lists:zip(lists:seq(0, NM-1), MatchSpecs)),

    Insts2 = lists:append(Insts ++ Insts1),
    {Insts2, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}};
transform_matchspec({match_list, [], B}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    transform_matchspec(B, {VarNum, EndLabel, NLabel, NVar, VarNameMap});
transform_matchspec({match_list, [H|T], B}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [ {literal, {funref, none, {std, is_list, 1}}, NVar},
          {literal, {funref, none, {std, head, 1}}, NVar+1},
          {literal, {funref, none, {std, tail, 1}}, NVar+2},
          {call, NVar, [VarNum], NVar+3},
          {branch, NVar+3, EndLabel},
          {call, NVar+1, [VarNum], NVar+4},
          {call, NVar+2, [VarNum], NVar+5} ],

    {Insts1, {_, EndLabel, NLabel1, NVar1, VarNameMap}} =
        transform_matchspec(H, {NVar+1, EndLabel, NLabel, NVar+6, VarNameMap}),

    {Insts2, {_, EndLabel, NLabel2, NVar2, VarNameMap}} =
        transform_matchspec({match_list, T, B}, {NVar+5, EndLabel, NLabel1, NVar1, VarNameMap}),

    Insts3 = Insts ++ Insts1 ++ Insts2,

    {Insts3, {VarNum, EndLabel, NLabel2, NVar2, VarNameMap}};
transform_matchspec({match_ignore, _}, State) ->
    {[], State}.


transform_match_tuple_element({N,MatchSpec}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [{literal, {funref, none, {std, get_element, 2}}, NVar},
         {literal, {integer, none, N}, NVar+1},
         {call, NVar, [VarNum, NVar+1], NVar+2}],

    {Insts1, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        transform_matchspecs({NVar+2, MatchSpec}, {EndLabel, NLabel, NVar+3, VarNameMap}),

    Insts2 = Insts ++ Insts1,
    {Insts2, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}}.


transform_matchspecs({VarNum, MatchSpecs}, {EndLabel, NLabel, NVar, VarNameMap}) ->
    {Insts, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(fun transform_matchspec/2, {VarNum, EndLabel, NLabel, NVar, VarNameMap}, MatchSpecs),
    {lists:append(Insts), {EndLabel, NLabel1, NVar1, VarNameMap}}.


transform_clause({MatchSpecs, [], []}, {OutLabel, OutVar, [VarNum], NLabel, NVar, VarNameMap}) ->
    {Insts, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(fun transform_matchspecs/2, {NLabel, NLabel+1, NVar, VarNameMap}, lists:zip([VarNum], MatchSpecs)),
    Insts1 = [[ {move, VarNum, OutVar}, {jump, OutLabel}, {label, EndLabel}]],
    Insts2 = lists:append(Insts ++ Insts1),
    {Insts2, {OutLabel, OutVar, [VarNum], NLabel1, NVar1, VarNameMap}};


transform_clause({MatchSpecs, Guards, Expressions}, {OutLabel, OutVar, VarNums, NLabel, NVar, VarNameMap}) ->
    {Insts, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(fun transform_matchspecs/2, {NLabel, NLabel+1, NVar, VarNameMap}, lists:zip(VarNums, MatchSpecs)),

    {Insts1, {EndLabel, NLabel2, NVar2, VarNameMap}} =
        lists:mapfoldl(fun transform_guard/2, {EndLabel, NLabel1, NVar1, VarNameMap}, Guards),

    {Vars, Insts2, {NLabel3, NVar3, VarNameMap}} = transform_expressions(Expressions, {NLabel2, NVar2, VarNameMap}),
    LastVar = lists:last(Vars),
    Insts3 =
        lists:append(
          Insts ++ Insts1 ++ Insts2
          ++ [[ {move, LastVar, OutVar}, {jump, OutLabel}, {label, EndLabel} ]]),
    {Insts3, {OutLabel, OutVar, VarNums, NLabel3, NVar3, VarNameMap}}.
