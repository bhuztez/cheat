-module(cheat_ccodegen).

-export([codegen/2]).

codegen({Entry, NIFAtomList, AtomList, NFList, FNList, _LitList}, Paths) ->
    FNList1 = lists:zip(lists:seq(0, length(FNList)-1), FNList),
    FileNames = lists:usort([FileName || {_, _, FileName, _} <- NFList ]),

    [read_file(Paths, "alloc.c"),
     read_file(Paths, "header.c"),
     [io_lib:format("static T const a_~s = ~w;~n", [Atom, N]) || {Atom, {a, N}} <- NIFAtomList],
     [ read_file(Paths, FileName) || FileName <- FileNames ],
     "#line 1 \"main.c\"\n",
     "char *ATOM[] = {\n  NULL,\n",
     [io_lib:format("  \"~s\",~n", [Atom]) || Atom <- AtomList],
     "};\n",
     "T (*N[])() = {\n",
     [io_lib:format("  ~w,~n", [NF]) || {_, NF, _, _} <- NFList],
     "};\n",
     "int main(){\n",
     "struct fun FN[] = {\n",
     [io_lib:format("  {&&f~w, ~w, ~w},~n", [FN, NArg, NVar]) || {FN, {fundef, _, NArg, _, NVar, _}}  <- FNList1],
     "};\n",
     io_lib:format("F=FN;~nT result;~nC(result, F(~w));~nreturn 0;~n", [Entry]),
     [ gen_fn(FN) || FN <- FNList1 ],
     "}\n"
    ].

read_file(Paths, FileName) ->
    {ok, Bin} = cheat_utils:path_read_file(Paths, FileName),
    [io_lib:format("#line 1 \"~s\"~n", [FileName]), Bin].

gen_fn({FN, {fundef, _, _, _, _, Insts}}) ->
    [io_lib:format("f~w:~n",[FN])] ++ [gen_inst(Inst, FN) || Inst <- Insts].

gen_inst({call, {nf, N}, Args, Result}, _) ->
    io_lib:format(
      "  ~s=N[~w](~s);\n",
      [gen_var(Result),
       N,
       gen_vars(Args)]);
gen_inst({call, Fun, Args, Result}, _) ->
    io_lib:format(
      "  C(~s,~s,~s);\n",
      [gen_var(Result),
       gen_var(Fun),
       gen_vars(Args)]);
gen_inst({move, S, T}, _) ->
    io_lib:format("  ~s=~s;~n", [gen_var(T),gen_var(S)]);
gen_inst({label, L}, N) ->
    io_lib:format("l~w_~w:~n", [N,L]);
gen_inst({jump, L}, N) ->
    io_lib:format("  goto l~w_~w;~n", [N,L]);
gen_inst(badmatch, _) ->
    "  BADMATCH;\n";
gen_inst({branch, V, L}, N) ->
    io_lib:format("  BR(~s) l~w_~w;~n", [gen_var(V), N,L]);
gen_inst({return, V}, _) ->
    io_lib:format("  RET(~s);~n", [gen_var(V)]).


gen_var(nil) ->
    "nil";
gen_var({integer, N}) ->
    io_lib:format("I(~w)", [N]);
gen_var({a,N}) ->
    io_lib:format("A(~w)", [N]);
gen_var({v,N}) ->
    io_lib:format("V(~w)", [N]);
gen_var({l,N}) ->
    io_lib:format("L[~w]", [N]);
gen_var({nf, N}) ->
    io_lib:format("N(~w)", [N]);
gen_var({fn, N}) ->
    io_lib:format("F(~w)", [N]).

gen_vars(Vars) ->
    string:join([gen_var(V) || V <- Vars], ",").
