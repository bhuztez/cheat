-module(argument_SUITE).

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [ test_no_options,
      test_libdir,
      test_emit,
      test_no_filename
    ].

test_no_options(_Config) ->
    test_argument(
      "x.cht",
      ["x.cht"],
      []).

test_libdir(_Config) ->
    test_argument(
      "x.cht",
      ["-L", "x.cht"],
      []),
    test_argument(
      "x.cht",
      ["-L", "libs", "x.cht"],
      ["libs"]).

test_emit(_Config) ->
    test_argument(
      "x.cht",
      ["-emit", "kernel", "x.cht"],
      [],
      kernel),
    test_argument(
      "x.cht",
      ["-L", "-emit", "kernel", "x.cht"],
      [],
      kernel).

test_no_filename(_Config) ->
    test_argument(none, [], []),
    test_argument(none, ["-no-such-opt"], []).

test_argument(FileName, Arguments, LibDirs) ->
    LibDir = code:lib_dir(cheat, lib),
    {FileName, [{libdir, [LibDir|LibDirs]}]} =
        cheat_argument:parse(Arguments),
    ok.

test_argument(FileName, Arguments, LibDirs, Emit) ->
    LibDir = code:lib_dir(cheat, lib),
    {FileName, [{emit, Emit}, {libdir, [LibDir|LibDirs]}]} =
        cheat_argument:parse(Arguments),
    ok.
