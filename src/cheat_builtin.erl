-module(cheat_builtin).

-export([get_function/1]).


%% intrinsic
get_function({io, print, 1}) ->
    {bif, print, "print.c"}.
