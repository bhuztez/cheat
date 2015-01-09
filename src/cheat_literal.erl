-module(cheat_literal).

-export([transform/1]).


transform(Forms) ->
    [transform_form(Form) || Form <- Forms].


transform_form({fundef, F, NArg, NNonLocal, Insts}) ->
    NumMap = dict:from_list([{N, {v,N}} || N <- lists:seq(0, NArg+NNonLocal-1)]),

    {Insts1, {NVar, _, _, LiteralMap}} =
        lists:mapfoldl(
          fun transform_instruction/2,
          {NArg+NNonLocal, 0, NumMap, dict:new()},
          Insts),

    Consts = [L ||{_,L} <- lists:usort([{C,V} || {V,{c,C}} <- dict:to_list(LiteralMap)])],
    {fundef, F, NArg, NNonLocal, NVar, Consts, lists:append(Insts1)}.


transform_instruction({call, Fun, Args, Result}, {NVar, NConst, NumMap, LiteralMap}) ->
    Args1 = [ dict:fetch(Arg, NumMap) || Arg <- Args],
    {[{call, dict:fetch(Fun, NumMap), Args1, {v,NVar}}],
     {NVar+1, NConst, dict:store(Result, {v,NVar}, NumMap), LiteralMap}};

transform_instruction({literal, {Type, _, Value}, Result}, {NVar, NConst, NumMap, LiteralMap}) ->
    case dict:find({Type, Value}, LiteralMap) of
        {ok, N} ->
            {[], {NVar, NConst, dict:store(Result, {c,N}, NumMap), LiteralMap}};
        error ->
            {[], {NVar, NConst+1,
                  dict:store(Result, {c,NConst}, NumMap),
                  dict:store({Type,Value}, {c, NConst}, LiteralMap)}}
    end;
transform_instruction({move, NSource, NTarget}, {NVar, NConst, NumMap, LiteralMap}) ->
    {[{move, dict:fetch(NSource, NumMap), {v,NVar}}],
     {NVar+1, NConst, dict:store(NTarget, {v,NVar}, NumMap), LiteralMap}};
transform_instruction({label, _}=Inst, State) ->
    {[Inst], State};
transform_instruction({jump, _}=Inst, State) ->
    {[Inst], State};
transform_instruction(badmatch, State) ->
    {[badmatch], State};
transform_instruction({branch, N, Label}, {_,_,NumMap,_}=State) ->
    {[{branch, dict:fetch(N,NumMap), Label}], State};
transform_instruction({return, N}, {_,_,NumMap,_}=State) ->
    {[{return, dict:fetch(N,NumMap)}], State}.
