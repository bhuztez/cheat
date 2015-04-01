-module(cheat_linearize).

-export([transform/2]).


transform(Forms, _) ->
    [transform_form(Form) || Form <- Forms].


transform_form({fundef, Line, {M, F, A}, NonLocal, Local, Expr}) ->
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

    {{VarNum, Insts}, {_, _NVar, VarNameMap}} =
        transform_expr(Expr, {0, NNL+A+NL, VarNameMap}),
    Insts1 = Insts ++ [{return, VarNum}],
    {fundef, Line, {M, F, A}, NNL, Insts1}.


transform_expr({arg, _}=Arg, {_, _, VarNameMap}=State) ->
    {{dict:fetch(Arg, VarNameMap), []}, State};
transform_expr({varname, Line, VarName}, {_, _, VarNameMap}=State) ->
    {{dict:fetch(VarName, VarNameMap), [{line, Line}]}, State};
transform_expr({literal, Line, nil},
               {NLabel, NVar, VarNameMap}) ->
    {{NVar, [{line, Line}, {literal, nil, NVar}]},
     {NLabel, NVar+1, VarNameMap}};
transform_expr({literal, Line, {Type, _, Literal}},
               {NLabel, NVar, VarNameMap}) ->
    {{NVar, [{line, Line}, {literal, {Type, Literal}, NVar}]},
     {NLabel, NVar+1, VarNameMap}};
transform_expr({make_fun, Line, {{atom,_,M},{atom,_,F},{integer,_,A}}},
               {NLabel, NVar, VarNameMap}) ->
    {{NVar, [{line, Line}, {literal, {funref,{M,F,A}}, NVar}]},
     {NLabel, NVar+1, VarNameMap}};

transform_expr({make_fun, Line, {M,F,A,[]}},
               {NLabel, NVar, VarNameMap}) ->
    {{NVar, [{line, Line}, {literal, {funref,{M,F,A}}, NVar}]},
     {NLabel, NVar+1, VarNameMap}};

transform_expr({make_fun, Line, {M,F,A,VarNames}}, {NLabel, NVar, VarNameMap}) ->
    VarNums = [ dict:fetch(VarName, VarNameMap) || VarName <- VarNames ],
    {{NVar+2,
      [{line, Line},
       {literal, {funref, {std, make_fun, length(VarNums)+1}}, NVar},
       {literal, {funref, {M,F,A}}, NVar+1},
       {call, NVar, [NVar+1|VarNums], NVar+2}]},
     {NLabel, NVar+3, VarNameMap}};

transform_expr({call, Line, {Fun, Exprs}}, State) ->
    {[FunNum|VarNums], Insts, {NLabel, NVar, VarNameMap}} =
        transform_exprs([Fun|Exprs], State),
    Insts1 =
        [{line, Line},
         {call, FunNum, VarNums, NVar}],
    {{NVar, Insts ++ Insts1},
     {NLabel, NVar+1, VarNameMap}};

transform_expr({'case', Line, {Exprs, Clauses}}, State) ->
    {VarNums, Insts, {NLabel, NVar, VarNameMap}} = transform_exprs(Exprs, State),
    {Insts1, {OutLabel, OutVar, VarNums, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(
          fun transform_clause/2,
          {NLabel, NVar, VarNums, NLabel+1, NVar+1, VarNameMap},
          Clauses),

    Insts2 = Insts ++ lists:append(Insts1) ++ [{line, Line}, badmatch, {label, OutLabel}],
    {{OutVar, Insts2}, {NLabel1, NVar1, VarNameMap}}.

transform_exprs(Exprs, State) ->
    {VarNumsAndInsts, State1} = lists:mapfoldl(fun transform_expr/2, State, Exprs),
    {VarNums, Insts} = lists:unzip(VarNumsAndInsts),
    {VarNums, lists:append(Insts), State1}.



transform_clause({MSs, [], []}, {OutLabel, OutVar, [VarNum], NLabel, NVar, VarNameMap}) ->
    {Insts, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(
          fun transform_mss/2,
          {NLabel, NLabel+1, NVar, VarNameMap},
          lists:zip([VarNum], MSs)),
    Insts1 = [ {move, VarNum, OutVar}, {jump, OutLabel}, {label, EndLabel}],
    Insts2 = lists:append(Insts) ++ Insts1,
    {Insts2, {OutLabel, OutVar, [VarNum], NLabel1, NVar1, VarNameMap}};


transform_clause({MSs, Guards, Exprs}, {OutLabel, OutVar, VarNums, NLabel, NVar, VarNameMap}) ->
    {Insts, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(fun transform_mss/2, {NLabel, NLabel+1, NVar, VarNameMap}, lists:zip(VarNums, MSs)),

    {Insts1, {EndLabel, NLabel2, NVar2, VarNameMap}} =
        lists:mapfoldl(fun transform_guard/2, {EndLabel, NLabel1, NVar1, VarNameMap}, Guards),

    {Vars, Insts2, {NLabel3, NVar3, VarNameMap}} = transform_exprs(Exprs, {NLabel2, NVar2, VarNameMap}),
    LastVar = lists:last(Vars),
    Insts3 =
        lists:append(Insts ++ Insts1) ++ Insts2
        ++ [ {move, LastVar, OutVar}, {jump, OutLabel}, {label, EndLabel} ],
    {Insts3, {OutLabel, OutVar, VarNums, NLabel3, NVar3, VarNameMap}}.



transform_guard(Guard, {EndLabel, NLabel, NVar, VarNameMap}) ->
    {{VarNum, Insts}, {NLabel1, NVar1, VarNameMap}} =
        transform_expr(Guard, {NLabel, NVar, VarNameMap}),
    Insts1 = Insts ++ [{branch, VarNum, EndLabel}],
    {Insts1, {EndLabel, NLabel1, NVar1, VarNameMap}}.


transform_ms({assign, Line, VarName}, {VarNum, _, _, _, VarNameMap}=State) ->
    VarNum1 = dict:fetch(VarName, VarNameMap),
    {[{line, Line}, {move, VarNum, VarNum1}], State};
transform_ms({match_var, Line, VarName}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    VarNum1 = dict:fetch(VarName, VarNameMap),
    Insts =
        [ {line, Line},
          {literal, {funref, {std, match, 2}}, NVar},
          {call, NVar, [VarNum, VarNum1], NVar+1},
          {branch, NVar+1, EndLabel} ],
    {Insts, {VarNum, EndLabel, NLabel, NVar+2, VarNameMap}};
transform_ms({match_literal, Line, nil}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [{line, Line},
         {literal, nil, NVar},
         {literal, {funref, {std, match, 2}}, NVar+1},
         {call, NVar+1, [VarNum, NVar], NVar+2},
         {branch, NVar+2, EndLabel}],
    {Insts, {VarNum, EndLabel, NLabel, NVar+3, VarNameMap}};
transform_ms({match_literal, Line, {Type, _, Literal}}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [{line, Line},
         {literal, {Type, Literal}, NVar},
         {literal, {funref, {std, match, 2}}, NVar+1},
         {call, NVar+1, [VarNum, NVar], NVar+2},
         {branch, NVar+2, EndLabel}],
    {Insts, {VarNum, EndLabel, NLabel, NVar+3, VarNameMap}};
transform_ms({match_tuple, Line, MSs}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    NM = length(MSs),
    Insts =
        [{line, Line},
         {literal, {funref, {std, is_tuple, 2}}, NVar},
         {literal, {integer, NM}, NVar+1},
         {call, NVar, [VarNum, NVar+1], NVar+2},
         {branch, NVar+2, EndLabel}],

    {Insts1, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(
          fun transform_match_tuple_element/2,
          {VarNum, EndLabel, NLabel, NVar+3, VarNameMap},
          lists:zip(lists:seq(1, NM), MSs)),

    Insts2 = Insts ++ lists:append(Insts1),
    {Insts2, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}};

transform_ms({match_list, _, {[], B}}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    {Insts, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        transform_mss({VarNum,B}, {EndLabel, NLabel, NVar, VarNameMap}),
    {Insts, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}};
transform_ms({match_list, Line, {[H|T], B}}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [ {line, Line},
          {literal, {funref, {std, is_cons, 1}}, NVar},
          {literal, {funref, {std, head, 1}}, NVar+1},
          {literal, {funref, {std, tail, 1}}, NVar+2},
          {call, NVar, [VarNum], NVar+3},
          {branch, NVar+3, EndLabel},
          {call, NVar+1, [VarNum], NVar+4},
          {call, NVar+2, [VarNum], NVar+5} ],

    {Insts1, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        transform_mss({NVar+4,H}, {EndLabel, NLabel, NVar+6, VarNameMap}),

    {Insts2, {_, EndLabel, NLabel2, NVar2, VarNameMap}} =
        transform_ms({match_list, Line, {T, B}}, {NVar+5, EndLabel, NLabel1, NVar1, VarNameMap}),

    Insts3 = Insts ++ Insts1 ++ Insts2,

    {Insts3, {VarNum, EndLabel, NLabel2, NVar2, VarNameMap}};
transform_ms({match_ignore, _, _}, State) ->
    {[], State}.


transform_match_tuple_element({N,MS}, {VarNum, EndLabel, NLabel, NVar, VarNameMap}) ->
    Insts =
        [{literal, {funref, {std, element, 2}}, NVar},
         {literal, {integer, N}, NVar+1},
         {call, NVar, [NVar+1, VarNum], NVar+2}],

    {Insts1, {EndLabel, NLabel1, NVar1, VarNameMap}} =
        transform_mss({NVar+2, MS}, {EndLabel, NLabel, NVar+3, VarNameMap}),

    Insts2 = Insts ++ Insts1,
    {Insts2, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}}.


transform_mss({VarNum, MSs}, {EndLabel, NLabel, NVar, VarNameMap}) ->
    {Insts, {VarNum, EndLabel, NLabel1, NVar1, VarNameMap}} =
        lists:mapfoldl(
          fun transform_ms/2,
          {VarNum, EndLabel, NLabel, NVar, VarNameMap},
          MSs),
    {lists:append(Insts), {EndLabel, NLabel1, NVar1, VarNameMap}}.
