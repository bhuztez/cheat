-module(cheat_builtin).

-export([get_function/1]).


%% intrinsic
get_function({io, print, 1}) ->
    {bif, print, "print.c"};
get_function({std, plus, 2}) ->
    {bif, plus, "plus.c"};

get_function({std, head, 1}) ->
    {bif, head, "head.c"};
get_function({std, tail, 1}) ->
    {bif, tail, "tail.c"};
get_function({std, is_cons, 1}) ->
    {bif, is_cons, "is_cons.c"};
get_function({std, cons, 2}) ->
    {bif, cons, "cons.c"};

get_function({std, element, 2}) ->
    {bif, element, "element.c"};
get_function({std, is_tuple, 2}) ->
    {bif, is_tuple, "is_tuple.c"};
get_function({std, make_tuple, N}) when N > 1 ->
    {bif, make_tuple, "make_tuple.c"};

get_function({std, match, 2}) ->
    {bif, match, "match.c"}.
