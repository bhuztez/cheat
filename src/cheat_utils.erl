-module(cheat_utils).

-export([path_read_file/2]).

path_read_file([Path|Paths], FileName) ->
    case file:read_file(filename:join(Path, FileName)) of
        {ok, Binary} ->
            {ok, Binary};
        _ ->
            path_read_file(Paths, FileName)
    end.
