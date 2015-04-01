-module(cheat_module).

-export([load_functions/2]).


load_functions(MainModule, Options) ->
    load_functions([{MainModule, main, 0}], Options, dict:new(), dict:new()).


load_functions([], _, _, LoadedFunctions) ->
    [Fun || {_, Fun} <- dict:to_list(LoadedFunctions)];
load_functions([H|T], Options, LoadedModules, LoadedFunctions) ->
    case dict:find(H, LoadedFunctions) of
        {ok, _} ->
            load_functions(T, Options, LoadedModules, LoadedFunctions);
        error ->
            {Fun = {_, #{consts:=Consts}}, LoadedModules1} =
                load_function(H, Options, LoadedModules),
            load_functions(
              [F || {funref, F} <- Consts]++T,
              Options,
              LoadedModules1,
              dict:store(H, Fun, LoadedFunctions))
    end.


load_function({M, _, _}=F, Options, LoadedModules) ->
    {Module, LoadedModules1} = find_module(M, Options, LoadedModules),
    Function =
        case Module of
            builtin ->
                cheat_builtin:get_function(F);
            Mod ->
                dict:fetch(F, Mod)
        end,
    {Function, LoadedModules1}.


find_module(ModuleName, Options, LoadedModules) ->
    case dict:find(ModuleName, LoadedModules) of
        {ok, Mod} ->
            {Mod, LoadedModules};
        error ->
            {ok, Mod} = read_module(ModuleName, Options),
            {Mod, dict:store(ModuleName, Mod, LoadedModules)}
    end.


read_module(std, _) ->
    {ok, builtin};
read_module(io, _) ->
    {ok, builtin};
read_module(gc, _) ->
    {ok, builtin};
read_module(ModuleName, Options = #{libdir:=Paths}) ->
    {ok, File, FileName} = file:path_open(Paths, [ModuleName, ".cht"], [read]),
    Forms = cheat_compile:compile1(
              File,
              Options#{emit := ir, filename := FileName}),
    {ok, dict:from_list(
           [{Name, {fundef, Form}}
            || Form = #{name := Name} <- Forms])}.
