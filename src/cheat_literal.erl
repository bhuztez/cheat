-module(cheat_literal).

-export([transform/2, filter_duplicate_lines/2]).


transform(Forms, #{filename := FileName}) ->
    [transform_form(Form, FileName) || Form <- Forms].


transform_form({fundef, Line, {_,_,NArg}=Name, NNonLocal, Insts}, FileName) ->
    NumMap = dict:from_list([{N, {v,N}} || N <- lists:seq(0, NArg+NNonLocal-1)]),

    {Insts1, {NVar, _, _, LiteralMap}} =
        lists:mapfoldl(
          fun transform_instruction/2,
          {NArg+NNonLocal, 0, NumMap, dict:new()},
          Insts),

    Insts2 = filter_consective_lines(lists:append(Insts1)),
    Insts3 = filter_duplicate_lines(Insts2, none),

    Consts = [L ||{_,L} <- lists:usort([{C,V} || {V,{c,C}} <- dict:to_list(LiteralMap)])],
    #{name      => Name,
      filename  => FileName,
      line      => Line,
      nnonlocal => NNonLocal,
      nvar      => NVar,
      consts    => Consts,
      insts     => Insts3}.

filter_consective_lines([]) ->
    [];
filter_consective_lines([{line, _}, L={line, _}|T]) ->
    filter_consective_lines([L|T]);
filter_consective_lines([H|T]) ->
    [H|filter_consective_lines(T)].

filter_duplicate_lines([], _) ->
    [];
filter_duplicate_lines([{line, Line}|T], Line) ->
    filter_duplicate_lines(T, Line);
filter_duplicate_lines([{line, Line}=H|T], none) ->
    [H|filter_duplicate_lines(T, Line)];
filter_duplicate_lines([{line, Line}=H|T], _) ->
    [H|filter_duplicate_lines(T, Line)];
filter_duplicate_lines([badmatch|T], _) ->
    [badmatch|filter_duplicate_lines(T, none)];
filter_duplicate_lines([return|T], _) ->
    [return|filter_duplicate_lines(T, none)];
filter_duplicate_lines([H|T], Line) ->
    [H|filter_duplicate_lines(T, Line)].


transform_instruction({call, Fun, Args, Result}, {NVar, NConst, NumMap, LiteralMap}=State) ->
    Args1 = [ dict:fetch(Arg, NumMap) || Arg <- Args],
    case dict:find(Result, NumMap) of
        {ok, V} ->
            {[{call, dict:fetch(Fun, NumMap), Args1, V}], State};
        error ->
            {[{call, dict:fetch(Fun, NumMap), Args1, {v,NVar}}],
             {NVar+1, NConst, dict:store(Result, {v,NVar}, NumMap), LiteralMap}}
    end;

transform_instruction({literal, Literal, Result}, {NVar, NConst, NumMap, LiteralMap}) ->
    case dict:find(Literal, LiteralMap) of
        {ok, N} ->
            {[], {NVar, NConst, dict:store(Result, N, NumMap), LiteralMap}};
        error ->
            {[], {NVar, NConst+1,
                  dict:store(Result, {c, NConst}, NumMap),
                  dict:store(Literal, {c, NConst}, LiteralMap)}}
    end;
transform_instruction({move, NSource, NTarget}, {NVar, NConst, NumMap, LiteralMap}=State) ->
    case dict:find(NTarget, NumMap) of
        {ok, V} ->
            {[{move, dict:fetch(NSource, NumMap), V}], State};
        error ->
            {[{move, dict:fetch(NSource, NumMap), {v,NVar}}],
             {NVar+1, NConst, dict:store(NTarget, {v,NVar}, NumMap), LiteralMap}}
    end;
transform_instruction({line, _}=Inst, State) ->
    {[Inst], State};
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
