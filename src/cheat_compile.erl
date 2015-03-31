-module(cheat_compile).

-export([compile_cmdline/0, compile/2, compile1/3]).

-import(filename, [dirname/1, basename/2, join/1]).


compile_cmdline() ->
    case cheat_argument:parse(init:get_plain_arguments()) of
        {none, _} ->
            io:format("ERROR: missing filename~n"), 
            halt(1);
        {FileName, Options} ->
            compile(FileName, Options),
            halt(0)
    end.


compile(FileName, Options) ->
    compile(proplists:lookup(emit, Options), FileName, Options).


module_name(FileName) ->
    list_to_atom(filename:basename(FileName, ".cht")).

libdirs(FileName, Options) ->
    [filename:dirname(FileName)|proplists:get_value(libdir, Options)].


compile({emit, Format}, FileName, Options) ->
    io:format("~p~n", [compile1(Format, FileName, Options)]);
compile(none, FileName, Options) ->
    LibDirs = libdirs(FileName, Options),
    ByteCode = compile1(bc, FileName, Options),
    io:format("~s", [cheat_ccodegen:codegen(ByteCode, LibDirs)]).


compile1(ast, FileName, _) ->
    cheat_parse:file(FileName);
compile1(core, FileName, Options) ->
    cheat_scope:transform(
      compile1(ast, FileName, Options),
      module_name(FileName));
compile1(kernel, FileName, Options) ->
    cheat_linearize:transform(
      compile1(core, FileName, Options));
compile1(ir, FileName, Options) ->
    cheat_literal:transform(
      compile1(kernel, FileName, Options));
compile1(funs, FileName, Options) ->
    ModuleName = module_name(FileName),
    LibDirs = libdirs(FileName, Options),
    cheat_module:load_functions(ModuleName, LibDirs);
compile1(bc, FileName, Options) ->
    ModuleName = module_name(FileName),
    cheat_constant:transform(
      ModuleName,
      compile1(funs, FileName, Options)).
