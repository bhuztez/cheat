-module(cheat_constant).

-export([transform/1]).


transform(Functions) ->
    BIFS = [ Fun || {_, {bif, _, _}} = Fun <- Functions],
    FunDefs = [ Fun || {_, {fundef, _, _, _, _, _, _}} = Fun <- Functions ],

    {Funs1, {_NAtom, NConst, AtomMap, ConstMap}} =
        lists:mapfoldl(
          fun transform_function/2,
          {2,0,dict:from_list([{false, 0}, {true, 1}]),dict:new()},
          FunDefs),

    Atoms = [A || {_,A} <- lists:usort([{K,V} || {V,K} <- dict:to_list(AtomMap)])],
    Consts = lists:usort([{K,V} || {V,K} <- dict:to_list(ConstMap)]),

    {BIFS, Funs1, Atoms, NConst, Consts}.


transform_function({Name, {fundef, F, NArg, NNonLocal, NVar, Consts, Insts}}, State) ->
    {Consts1, State1} =
        lists:mapfoldl(
          fun transform_const/2,
          State,
          Consts),

    {Insts1, Consts1} =
        lists:mapfoldl(
          fun transform_instruction/2,
          Consts1,
          Insts),
    {{Name, {fundef, F, NArg, NNonLocal, NVar, Insts1}}, State1}.


transform_const({atom, Atom}, {NAtom, NConst, AtomMap, ConstMap}) ->
    case dict:find(Atom, AtomMap) of
        {ok, N} ->
            {{a, N}, {NAtom, NConst, AtomMap, ConstMap}};
        error ->
            {{a, NAtom}, {NAtom+1, NConst, dict:store(Atom, NAtom, AtomMap), ConstMap}}
    end;
transform_const(Const, {NAtom, NConst, AtomMap, ConstMap}) ->
    case dict:find(Const, ConstMap) of
        {ok, N} ->
            {{c, N}, {NAtom, NConst, AtomMap, ConstMap}};
        error ->
            {{c, NConst}, {NAtom, NConst+1, AtomMap, dict:store(Const, NConst, ConstMap)}}
    end.


get_const({c, N}, Consts) ->
    lists:nth(N+1, Consts);
get_const(V, _) ->
    V.

get_consts(Vars, Consts) ->
    [get_const(V, Consts) || V <- Vars ].

transform_instruction({call, Fun, Args, Result}, Consts) ->
    { {call,
       get_const(Fun, Consts),
       get_consts(Args, Consts),
       get_const(Result, Consts)},
      Consts};
transform_instruction({move, Source, Target}, Consts) ->
    { {move,
       get_const(Source, Consts),
       get_const(Target, Consts)},
      Consts};
transform_instruction({label, _}=Inst, Consts) ->
    {Inst, Consts};
transform_instruction({jump, _}=Inst, Consts) ->
    {Inst, Consts};
transform_instruction(badmatch, Consts) ->
    {badmatch, Consts};
transform_instruction({branch, V, L}, Consts) ->
    { {branch, get_const(V, Consts), L}, Consts };
transform_instruction({return, V}, Consts) ->
    { {return, get_const(V, Consts)}, Consts}.
