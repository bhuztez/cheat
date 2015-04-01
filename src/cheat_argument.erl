-module(cheat_argument).

-export([parse/1]).


parse(Arguments) ->
    {Options, FileName} = parse_options(Arguments),

    LibDirs =
        [code:lib_dir(cheat, lib)|
         lists:append(proplists:get_all_values('L', Options))],
    Opts1 =
        case FileName of
            none ->
                #{libdir => LibDirs};
            _ ->
                Module =
                    list_to_atom(filename:basename(FileName, ".cht")),
                #{filename => FileName,
                  module => Module,
                  libdir => [filename:dirname(FileName)|LibDirs]}
        end,

    case proplists:lookup(emit, Options) of
        {emit, [H|_]} ->
            Opts1#{emit => list_to_atom(H)};
        _ ->
            Opts1
    end.


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
