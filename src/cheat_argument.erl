-module(cheat_argument).

-export([parse/1]).


parse(Arguments) ->
    {Options, FileName} = parse_options(Arguments),

    LibDirs = lists:append(proplists:get_all_values('L', Options)),
    Opts1 = [{libdir, [code:lib_dir(cheat, lib)|LibDirs]}],

    Opts2 =
        case proplists:lookup(emit, Options) of
            {emit, [H|_]} ->
                [{emit, list_to_atom(H)}|Opts1];
            _ ->
                Opts1
        end,

    {FileName, Opts2}.


parse_options([]) ->
    {[], none};
parse_options(["-"++Opt|T]) ->
    {Values, Rest} = parse_values(T),
    {Options, FileName} = parse_options(Rest),
    {[{list_to_atom(Opt),Values}|Options], FileName};
parse_options([FileName]) ->
    {[], FileName}.


parse_values([]) ->
    {[], []};
parse_values([_]=L) ->
    {[], L};
parse_values(["-"++_|_]=L) ->
    {[], L};
parse_values([V|T]) ->
    {VS, Remain} = parse_values(T),
    {[V|VS], Remain}.
