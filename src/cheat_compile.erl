-module(cheat_compile).

-export([compile/1]).

-import(filename, [dirname/1, basename/2, join/1]).

compile(Filename) ->
    Dir = dirname(dirname(code:which(?MODULE))),
    ModuleName = list_to_atom(basename(Filename, ".cht")),
    Functions =
        cheat_module:load_functions(
          ModuleName,
          [dirname(Filename), join([Dir, "lib"])]),

    Funs1 = cheat_constant:transform(Functions),
    Output = cheat_ccodegen:codegen(ModuleName, Funs1, join([Dir, "bif"])),
    io:format("~s", [Output]).
