#!/usr/bin/env escript
%%! -pz ebin

main([Filename]) ->
    cheat_compile:compile(Filename).
