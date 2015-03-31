-module(cheat_builtin).

-export([get_function/1]).


get_function({io, print, 1}) ->
    {nif, io_print, "io_print.c", []};
get_function({std, plus, 2}) ->
    {nif, std_plus, "std_plus.c", []};

get_function({std, head, 1}) ->
    {nif, std_head, "std_head.c", []};
get_function({std, tail, 1}) ->
    {nif, std_tail, "std_tail.c", []};
get_function({std, is_cons, 1}) ->
    {nif, std_is_cons, "std_is_cons.c", []};
get_function({std, cons, 2}) ->
    {nif, std_cons, "std_cons.c", []};

get_function({std, element, 2}) ->
    {nif, std_element, "std_element.c", []};
get_function({std, is_tuple, 2}) ->
    {nif, std_is_tuple, "std_is_tuple.c", []};
get_function({std, make_tuple, N}) when N >= 1 ->
    {nif, std_make_tuple, "std_make_tuple.c", []};

get_function({std, match, 2}) ->
    {nif, std_match, "std_match.c", []};
get_function({std, make_fun, N}) when N >= 1 ->
    {nif, std_make_fun, "std_make_fun.c", []}.
