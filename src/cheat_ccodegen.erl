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
     "T r;",
     io_lib:format("C(r,FN(~w))", [MainLabel]),
     "RT 0;",
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
    io_lib:format("F(~w,f~w,~w,~w)", [N, N, NArg, NVar]).

gen_const({N, {integer, I}}, _, _) ->
    io_lib:format("C[~w]=INT(~w);", [N, I]);
gen_const({N, {funref, F}}, BIFHeaderMap, FunHeaderMap) ->
    case dict:find(F, BIFHeaderMap) of
        {ok, NB} ->
            io_lib:format("C[~w]=BF(~w);", [N, NB]);
        error ->
            io_lib:format("C[~w]=FN(~w);", [N, dict:fetch(F, FunHeaderMap)])
    end.


gen_funbody({N, {fundef, _, _, _, _, Insts}}) ->
    [ io_lib:format("~nf~w:", [N]),
      [ gen_inst(Inst, N) || Inst <- Insts]
    ].


gen_inst({call, Fun, Args, Result}, _) ->
    io_lib:format(
      "C(~s,~s,~s)",
      [gen_var(Result),
       gen_var(Fun),
       gen_vars(Args)]);
gen_inst({move, S, T}, _) ->
    io_lib:format("M(~s,~s)", [gen_var(S),gen_var(T)]);
gen_inst({label, L}, N) ->
    io_lib:format("~nl~w_~w:", [N,L]);
gen_inst({jump, L}, N) ->
    io_lib:format("J(l~w_~w)", [N,L]);
gen_inst(badmatch, _) ->
    "E();";
gen_inst({branch, V, L}, N) ->
    io_lib:format("B(~s,l~w_~w)", [gen_var(V), N,L]);
gen_inst({return, V}, _) ->
    io_lib:format("R(~s)", [gen_var(V)]).


gen_var({a,N}) ->
    io_lib:format("A(~w)", [N]);
gen_var({v,N}) ->
    io_lib:format("V[~w]", [N]);
gen_var({c,N}) ->
    io_lib:format("C[~w]", [N]).

gen_vars(Vars) ->
    string:join([gen_var(V) || V <- Vars], ",").

