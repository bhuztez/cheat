-module(cheat_ccodegen).

-export([codegen/3]).


codegen(MainModule, {BIFS, Funs, Atoms, NConst, Consts}, BIFDir) ->
    NBIFS = length(BIFS),
    {BIFHeaders, BIFBodies} = lists:unzip(BIFS),
    BIFHeaderMap =
        dict:from_list(
          lists:zip(BIFHeaders, lists:seq(0,NBIFS-1))),

    NFuns = length(Funs),
    {FunHeaders, FunBodies} = lists:unzip(Funs),
    FunHeaderMap =
        dict:from_list(
          lists:zip(FunHeaders, lists:seq(0,NFuns-1))),
    Funs1 = lists:zip(lists:seq(0,NFuns-1), FunBodies),
    MainLabel = dict:fetch({MainModule, main,0}, FunHeaderMap),

    [include_file("prologue.c", BIFDir),
     [include_file(File, BIFDir) || {_,_,File}<- BIFBodies],
     "T (*BIF[])()={",
     string:join([gen_bif(B) || B <- BIFBodies], ","),
     "};\n",
     "char *ATOM[]={",
     string:join([gen_atom(A) || A <- Atoms], ","),
     "};\n",
     io_lib:format("T C[~w];", [NConst]),
     io_lib:format("fn_t FUN[~w];", [NFuns]),
     "\nint main() {\n",
     [ gen_fun(N) || N <- Funs1],
     [ gen_const(C, BIFHeaderMap, FunHeaderMap) || C <- Consts ],
     "\nT r;",
     io_lib:format("C(r,F(~w))", [MainLabel]),
     "RT 0;\n",
     [ gen_funbody(F) || F <- Funs1 ],
     "}\n"
    ].


include_file(File, BIFDir) ->
    {ok, Content} = file:read_file(filename:join([BIFDir,File])),
    Content.

gen_atom(Atom) ->
    io_lib:format("\"~s\"", [Atom]).

gen_bif({_, Name, _}) ->
    io_lib:format("~s", [Name]).

gen_fun({N, {fundef, _, NArg, _, NVar, _}}) ->
    io_lib:format("FN(~w,f~w,~w,~w)", [N, N, NArg, NVar]).

gen_const({N, nil}, _, _) ->
    io_lib:format("C[~w]=TN;", [N]);
gen_const({N, {integer, I}}, _, _) ->
    io_lib:format("C[~w]=I(~w);", [N, I]);
gen_const({N, {funref, F}}, BIFHeaderMap, FunHeaderMap) ->
    case dict:find(F, BIFHeaderMap) of
        {ok, NB} ->
            io_lib:format("C[~w]=P(~w);", [N, NB]);
        error ->
            io_lib:format("C[~w]=F(~w);", [N, dict:fetch(F, FunHeaderMap)])
    end.


gen_funbody({N, {fundef, _, _, _, _, Insts}}) ->
    [ io_lib:format("~nf~w:", [N]),
      [ gen_inst(Inst, N) || Inst <- Insts]
    ].


gen_inst({call, Fun, Args, Result}, _) ->
    io_lib:format(
      "C(~s,~s,~s)\n",
      [gen_var(Result),
       gen_var(Fun),
       gen_vars(Args)]);
gen_inst({move, S, T}, _) ->
    io_lib:format("M(~s,~s)~n", [gen_var(S),gen_var(T)]);
gen_inst({label, L}, N) ->
    io_lib:format("l~w_~w:~n", [N,L]);
gen_inst({jump, L}, N) ->
    io_lib:format("J(l~w_~w)~n", [N,L]);
gen_inst(badmatch, _) ->
    "X();\n";
gen_inst({branch, V, L}, N) ->
    io_lib:format("B(~s,l~w_~w)~n", [gen_var(V), N,L]);
gen_inst({return, V}, _) ->
    io_lib:format("R(~s)~n", [gen_var(V)]).


gen_var({a,N}) ->
    io_lib:format("A(~w)", [N]);
gen_var({v,N}) ->
    io_lib:format("V[~w]", [N]);
gen_var({c,N}) ->
    io_lib:format("C[~w]", [N]).

gen_vars(Vars) ->
    string:join([gen_var(V) || V <- Vars], ",").

