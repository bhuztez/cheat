-module(cheat_builtin).

-export([get_function/1]).


get_function({io, read_int, 0}=Name) ->
    get_function(Name, []);
get_function({io, print_int, 1}=Name) ->
    get_function(Name, []);

get_function({std, cons, 2}=Name) ->
    get_function(Name, []);
get_function({std, element, 2}=Name) ->
    get_function(Name, []);
get_function({std, gc, 0}=Name) ->
    get_function(Name, []);
get_function({std, head, 1}=Name) ->
    get_function(Name, []);
get_function({std, heap_info, 1}=Name) ->
    get_function(Name, [{atom, used}, {atom, free}, {atom, size}]);
get_function({std, match, 2}=Name) ->
    get_function(Name, []);
get_function({std, is_cons, 1}=Name) ->
    get_function(Name, []);
get_function({std, is_tuple, 2}=Name) ->
    get_function(Name, []);
get_function({std, plus, 2}=Name) ->
    get_function(Name, []);
get_function({std, tail, 1}=Name) ->
    get_function(Name, []);

get_function({std, make_tuple, N}=Name) ->
    FunName = "std_make_tuple_"++integer_to_list(N),
    Args1 = [io_lib:format("T a~w", [I]) || I <- lists:seq(1, N)],
    Args2 = [io_lib:format("a~w", [I]) || I <- lists:seq(1, N)],
    Code = [io_lib:format("T ~s(~s) {~n", [FunName, string:join(Args1, ",")]),
            io_lib:format("  return std_make_tuple(~s);~n", [string:join([integer_to_list(N)|Args2], ",")]),
            "}\n"
           ],
    {nif,
     #{name     => Name,
       cname    => FunName,
       filename => "std_make_tuple.c",
       consts   => [],
       code     => Code}}.


get_function({M,F,A}=Name, Consts) ->
    FullName = io_lib:format("~s_~s_~w", [M,F,A]),
    {nif,
     #{name     => Name,
       cname    => FullName,
       filename => [FullName, ".c"],
       consts   => Consts}}.
