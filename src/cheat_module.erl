-module(cheat_module).

-export([load_functions/2]).


load_functions(MainModule, Paths) ->
    load_functions([{MainModule, main, 0}], Paths, dict:new(), dict:new()).


load_functions([], _, _, LoadedFunctions) ->
    dict:to_list(LoadedFunctions);
load_functions([H|T], Paths, LoadedModules, LoadedFunctions) ->
    case dict:find(H, LoadedFunctions) of
        {ok, _} ->
            load_functions(T, Paths, LoadedModules, LoadedFunctions);
        error ->
            {Fun, LoadedModules1} = load_function(H, Paths, LoadedModules),
            load_functions(
              get_funrefs(Fun)++T,
              Paths,
              LoadedModules1,
              dict:store(H, Fun, LoadedFunctions))
    end.


module_name({M,_,_}) ->
    M;
module_name({M,_}) ->
    M.


fundef_name({_,F,A}) ->
    {fundef, F, A};
fundef_name({_,F}) ->
    {fundef, F}.


load_function(F, Paths, LoadedModules) ->
    ModuleName = module_name(F),
    {Module, LoadedModules1} = find_module(ModuleName, Paths, LoadedModules),
    Function =
        case Module of
            {cht, Mod} ->
                dict:fetch(fundef_name(F), Mod);
            builtin ->
                cheat_builtin:get_function(F)
        end,
    {Function, LoadedModules1}.


get_funrefs({fundef, _, _, _, _, Local, _}) ->
    [F || {funref, F} <- Local];
get_funrefs({bif, _, _}) ->
    [].


find_module(ModuleName, Paths, LoadedModules) ->
    case dict:find(ModuleName, LoadedModules) of
        {ok, Mod} ->
            {Mod, LoadedModules};
        error ->
            {ok, Mod} = read_module(ModuleName, Paths),
            {Mod, dict:store(ModuleName, Mod, LoadedModules)}
    end.


read_module(std, _) ->
    {ok, builtin};
read_module(io, _) ->
    {ok, builtin};
read_module(_, []) ->
    not_found;
read_module(ModuleName, [Path|Paths]) ->
    BaseName = filename:join(Path, atom_to_list(ModuleName)),
    case read_module(BaseName) of
        {ok, Module} ->
            {ok, {cht, Module}};
        _ ->
            read_module(ModuleName, Paths)
    end.


read_module(BaseName) ->
    case read_source_code(BaseName) of
        {ok, Bytecode} ->
            Module = dict:from_list([transform_form(Form) || Form <- Bytecode]),
            {ok, Module};
        _ ->
            not_found
    end.


read_source_code(BaseName) ->
    ModuleName = list_to_atom(filename:basename(BaseName)),
    case file:read_file(BaseName++".cht") of
        {ok, Content} ->
            Forms = cheat_parse:string(Content),
            Forms1 = cheat_scope:transform(Forms, ModuleName),
            Forms2 = cheat_linearize:transform(Forms1),
            {ok, cheat_literal:transform(Forms2)};
        {error, _} ->
            not_found
    end.


transform_form({fundef, F, NArg, _, _, _, _}=Form)
  when is_atom(F) ->
    {{fundef, F, NArg}, Form};
transform_form({fundef, F, _, _, _, _, _} = Form) ->
    {{fundef, F}, Form}.
