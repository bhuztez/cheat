-module(cheat_compile).

-export([compile_cmdline/0, compile/1, compile/2, compile1/2]).

compile_cmdline() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(
            cheat_compile, compile,
            [init:get_plain_arguments()]),
    receive
        {'EXIT',Pid, normal} ->
            halt(0);
        {'EXIT',Pid, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            halt(1)
    end.


compile(Arguments) ->
    case cheat_argument:parse(Arguments) of
        #{filename := FileName} = Options ->
            compile(FileName, Options)
    end.

compile(FileOrName, Options = #{emit := _}) ->
    io:format("~p~n", [compile1(FileOrName, Options)]);
compile(FileOrName, Options) ->
    io:format("~s~n", [compile1(FileOrName, Options)]).


compile1(File, #{emit := token}) when is_pid(File) ->
    {ok, Tokens, _} =
        io:request(File, {get_until,unicode,"",cheat_lexer,tokens,[]}),
    Tokens;
compile1(FileName, #{emit := token} = Options) ->
    {ok, File} = file:open(FileName, [read]),
    try
        compile1( File, Options)
    after
        file:close(File)
    end;
compile1(FileOrName, #{emit := form} = Options) ->
    {ok, Forms} =
        cheat_parser:parse(
          compile1(FileOrName, Options#{emit := token})),
    Forms;
compile1(FileOrName, #{emit := ast} = Options) ->
    cheat_parse:transform(
      compile1(FileOrName, Options#{emit := form}),
      Options);
compile1(FileOrName, #{emit := core} = Options) ->
    cheat_scope:transform(
      compile1(FileOrName, Options#{emit := ast}),
      Options);
compile1(FileOrName, #{emit := kernel} = Options) ->
    cheat_linearize:transform(
      compile1(FileOrName, Options#{emit := core}),
      Options);
compile1(FileOrName, #{emit := ir} = Options) ->
    cheat_literal:transform(
      compile1(FileOrName, Options#{emit := kernel}),
      Options);
compile1(_, #{emit := funs, module := Module} = Options) ->
    cheat_module:load_functions(Module, Options);
compile1(FileName, #{emit := bc} = Options) ->
    cheat_constant:transform(
      cheat_compile:compile1(FileName, Options#{emit := funs}),
      Options);
compile1(FileName, Options) ->
    cheat_ccodegen:transform(
      cheat_compile:compile1(FileName, Options#{emit => bc}),
      Options).
