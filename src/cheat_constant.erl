-module(cheat_constant).

-export([transform/2]).


transform(Functions, #{module := Module}) ->
    NIFS = [Fun || {nif, Fun} <- Functions ],
    FUNS = [Fun || {fundef, Fun} <- Functions ],

    ConstMap1 = number_const(0, [{funref, Name} || #{name := Name} <- NIFS], nf, dict:new()),
    ConstMap2 = number_const(0, [{funref, Name} || #{name := Name} <- FUNS], fn, ConstMap1),

    NIFLits = lists:append([Consts || #{consts := Consts} <- NIFS]),
    FUNLits = lists:append([Consts || #{consts := Consts} <- FUNS]),
    Lits = NIFLits ++ FUNLits,

    NIFAtoms = lists:usort([true,false] ++ [ X || {atom, X} <- NIFLits ]),
    Atoms = lists:usort([X || {atom,_}=X <- FUNLits ] ++ [{atom,X} || X <- NIFAtoms]),
    ConstMap3 = number_const(1, Atoms, a, ConstMap2),
    AtomList = [X || {_, X} <- Atoms],
    NIFAtomList = 
        [{A, N}
         || {A, {a, N}} <-
                [{X, dict:fetch({atom, X}, ConstMap3)}
                 || X <- NIFAtoms]],
    LitList =
        lists:usort(
          [ L
            || L <- Lits,
               case L of
                   nil -> false;
                   {atom, _} -> false;
                   {integer, _} -> false;
                   {funref, _} -> false;
                   _ -> true
               end]),
    ConstMap4 = number_literal(0, LitList, ConstMap3),
    FUNS1 = [ transform_fun(Fun, ConstMap4) || Fun <- FUNS],
    {fn, Entry} = dict:fetch({funref, {Module, main, 0}}, ConstMap4),
    {Entry, NIFAtomList, AtomList, NIFS, FUNS1, LitList}.


number_literal(_, [], ConstMap) ->
    ConstMap;
number_literal(N, [H|T], ConstMap) ->
    number_literal(N+1, T, dict:store(H, {l, N}, ConstMap)).

number_const(_, [], _, ConstMap) ->
    ConstMap;
number_const(N, [H|T], Type, ConstMap) ->
    number_const(N+1, T, Type, dict:store(H, {Type, N}, ConstMap)).


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

transform_fun(Fun = #{consts := Consts, insts := Insts}, ConstMap) ->
    Consts1 =
        dict:from_list(
          lists:zip(lists:seq(0, length(Consts)-1),
                    [map_const(Const, ConstMap) || Const <- Consts ])),

    Insts1 = [ transform_inst(Inst, Consts1) || Inst <- Insts ],
    Fun#{insts := Insts1}.

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
transform_inst({line, _}=Inst, _Consts) ->
    Inst;
transform_inst({jump, _}=Inst, _Consts) ->
    Inst;
transform_inst(badmatch, _Consts) ->
    badmatch;
transform_inst({branch, V, L}, Consts) ->
    {branch, get_const(V, Consts), L};
transform_inst({return, V}, Consts) ->
    {return, get_const(V, Consts)}.
