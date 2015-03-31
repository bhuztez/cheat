-module(cheat_constant).

-export([transform/2]).


number_nf(_, [], _, ConstMap) ->
    ConstMap;
number_nf(Count, [{F, {nif, Name, _, _}}|T], NIFMap, ConstMap) ->
    case dict:find(Name, NIFMap) of
        {ok, N} ->
            Count1 = Count,
            NIFMap1 = NIFMap;
        error ->
            Count1 = Count + 1,
            NIFMap1 = dict:store(Name, Count, NIFMap),
            N = Count
    end,

    number_nf(Count1, T, NIFMap1,
              dict:store({funref, F}, {nf, N}, ConstMap)).


number_fn(_, [], ConstMap) ->
    ConstMap;
number_fn(N, [{F, {fundef, _, _, _, _, _, _}}|T], ConstMap) ->
    number_fn(N+1, T,
              dict:store({funref, F}, {fn, N}, ConstMap)).

number_atom(_, [], ConstMap) ->
    ConstMap;
number_atom(N, [H|T], ConstMap) ->
    number_atom(N+1, T,
                dict:store({atom, H}, {a, N}, ConstMap)).

number_literal(_, [], ConstMap) ->
    ConstMap;
number_literal(N, [nil|T], ConstMap) ->
    number_literal(N, T, ConstMap);
number_literal(N, [{atom, _}|T], ConstMap) ->
    number_literal(N, T, ConstMap);
number_literal(N, [{integer, _}|T], ConstMap) ->
    number_literal(N, T, ConstMap);
number_literal(N, [{funref, _}|T], ConstMap) ->
    number_literal(N, T, ConstMap);
number_literal(N, [H|T], ConstMap) ->
    number_literal(N+1, T, dict:store(H, {l, N}, ConstMap)).


transform(MainModule, Functions) ->
    NIFDefs = [ Fun || {_, {nif, _, _, _}} = Fun <- Functions],
    FUNDefs = [ Fun || {_, {fundef, _, _, _, _, _, _}} = Fun <- Functions ],

    ConstMap =
        number_fn(0, FUNDefs, number_nf(0, NIFDefs, dict:new(), dict:new())),

    NIFLits = lists:append([ Local || {_, {nif, _, _, Local}} <- Functions ]),
    NIFAtoms = lists:usort([true, false] ++ [X || {atom, X} <- NIFLits ]),
    FUNLits = lists:append([ Local || {_, {fundef, _, _, _, _, Local, _}} <- Functions ]),
    Lits = NIFLits ++ FUNLits,

    LitList = [ L
                || L <- Lits,
                   case L of
                       nil -> false;
                       {atom, _} -> false;
                       {integer, _} -> false;
                       {funref, _} -> false;
                       _ -> true
                   end],


    AtomList = lists:usort(NIFAtoms ++ [X || {atom, X} <- FUNLits ]),
    ConstMap1 = number_atom(1, AtomList, ConstMap),
    ConstMap2 = number_literal(0, LitList, ConstMap1),

    NFList = [ Fun || {_, {nif, _, _, _} = Fun} <- Functions],
    FNList = [ transform_fun(Fun, ConstMap2) || {_, {fundef, _, _, _, _, _, _} = Fun} <- Functions ],
    NIFAtomList = [{A, dict:fetch({atom, A}, ConstMap2)} || A <- NIFAtoms ],
    {fn, Entry} = dict:fetch({funref, {MainModule, main, 0}}, ConstMap2),

    {Entry, NIFAtomList, AtomList, NFList, FNList, LitList}.


get_const({c, N}, Consts) ->
    dict:fetch(N, Consts);
get_const(V, _) ->
    V.

get_consts(Vars, Consts) ->
    [get_const(V, Consts) || V <- Vars ].

map_const(nil, _) ->
    nil;
map_const({integer, _}=N, _) ->
    N;
map_const(C, ConstMap) ->
    dict:fetch(C, ConstMap).

transform_fun({fundef, F, NArg, NNonLocal, NVar, Consts, Insts}, ConstMap) ->
    Consts1 =
        dict:from_list(
          lists:zip(lists:seq(0, length(Consts)-1),
                    [map_const(Const, ConstMap) || Const <- Consts ])),

    Insts1 = [ transform_inst(Inst, Consts1) || Inst <- Insts ],
    {fundef, F, NArg, NNonLocal, NVar, Insts1}.


transform_inst({call, Fun, Args, Result}, Consts) ->
    {call,
     get_const(Fun, Consts),
     get_consts(Args, Consts),
     get_const(Result, Consts)};
transform_inst({move, Source, Target}, Consts) ->
    {move,
     get_const(Source, Consts),
     get_const(Target, Consts)};
transform_inst({label, _}=Inst, _Consts) ->
    Inst;
transform_inst({jump, _}=Inst, _Consts) ->
    Inst;
transform_inst(badmatch, _Consts) ->
    badmatch;
transform_inst({branch, V, L}, Consts) ->
    {branch, get_const(V, Consts), L};
transform_inst({return, V}, Consts) ->
    {return, get_const(V, Consts)}.
