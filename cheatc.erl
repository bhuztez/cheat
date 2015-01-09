#!/usr/bin/env escript
%%! -pz ebin

main([Filename]) ->
    ModuleNameS = filename:basename(Filename, ".cht"),
    ModuleName = list_to_atom(ModuleNameS),
    Functions = cheat_module:load_functions(ModuleName, ["."]),
    Funs1 = cheat_constant:transform(Functions),
    Output = cheat_ccodegen:codegen(ModuleName, Funs1),
    io:format("~s~n", [Output]).
