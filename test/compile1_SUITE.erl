-module(compile1_SUITE).

-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [test_parse,
     test_scope,
     test_linearize,
     test_literal].

test_parse(Config) ->
    test_files(
      [ simple,
        multiple_clause,
        literal,
        matchspec,
        guard,
        fundef],
      ast, Config).

test_scope(Config) ->
    test_files(
      [ literal,
        matchspec,
        fundef,
        call,
        case_scope,
        nested_scope
      ],
      core, Config).

test_linearize(Config) ->
    test_files(
      [ literal,
        matchspec,
        guard,
        call,
        nested_scope
      ],
      kernel, Config).

test_literal(Config) ->
    test_files(
      [ literal,
        matchspec,
        guard,
        call,
        nested_scope
      ],
      ir, Config).


test_files(Files, Type, Config) ->
    [ test_compile1(Type, File, [], Config)
      || File <- Files ],
    ok.

test_compile1(Type, Filename, Options, Config) ->
    {ok, [Expected]} =
        file:consult(
          [?config(data_dir, Config), Filename, ".", Type]),
    Expected =
        cheat_compile:compile1(
          Type,
          [?config(data_dir, Config), Filename, ".cht"],
          Options),
    ok.
