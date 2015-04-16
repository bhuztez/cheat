-module(cheat_parse).

-export([transform/2]).


transform(Forms, #{module := Module}) ->
    [transform_form(Form, Module) || Form <- Forms].


transform_form({'fun', Clauses}, Module) ->
    {fundef, Line, {{atom, F}, A, Expr}} = transform_clauses(Clauses, Module),
    {fundef, Line, {F, A, Expr}}.


transform_clauses(Clauses, Module) ->
    [{clause, Line, _}|_] = Clauses1 =
        [transform_clause(Clause, Module) || Clause <- Clauses],
    {true, {F, A}} =
        all_same([{X,Y} || {clause, _, {X, Y, _, _, _}} <- Clauses1]),
    {fundef, Line,
     {F, A,
      {'case', Line,
       {[{arg, N} || N <- lists:seq(0, A-1)],
        [{M, G, E} || {clause, _, {_, _, M, G, E}}<- Clauses1]}}}}.


all_same([{F,A}|T]) ->
    all_same(T, {F,A}).

all_same([], X) ->
    {true, X};
all_same([X|T], X) ->
    all_same(T, X);
all_same(_, _) ->
    false.


transform_clause({clause, Line, {Name, MSs, Guards, Exprs}}, Module) ->
    {clause, Line,
     {clause_name(Name),
      length(MSs),
      [transform_ms(MS, Module) || MS <- MSs],
      [transform_expr(Guard, Module) || Guard <- Guards],
      [transform_expr(Expr, Module) || Expr <- Exprs]}}.


clause_name(ignore) ->
    ignore;
clause_name({varname, _, Name}) ->
    {varname, Name};
clause_name({atom, _, Atom}) ->
    {atom, Atom}.


transform_ms({match, _, {A, B}}, Module) ->
    transform_ms(A, Module) ++ transform_ms(B, Module);
transform_ms({tuple, Line, MSs}, Module) ->
    [{match_tuple, Line, [transform_ms(MS, Module) || MS <- MSs]}];
transform_ms({list, Line, {A, B}}, Module) ->
    [{match_list, Line,
      {[transform_ms(MS, Module)||MS <- A], transform_ms(B, Module)}}];
transform_ms({varname, Line, X}, _) ->
    [{match_var, Line, X}];
transform_ms({ignore, Line, X}, _) ->
    [{match_ignore, Line, X}];
transform_ms({literal, Line, X}, _) ->
    [{match_literal, Line, X}];
transform_ms({'fun', Line, {local,F,A}}, M) ->
    [{match_fun, Line, {M,F,A}}];
transform_ms({'fun', Line, {M,F,A}}, _) ->
    [{match_fun, Line, {M,F,A}}];
transform_ms({binary, Line, Fields}, Module) ->
    [{match_binary, Line,
      [{binary_field, L, {transform_ms(V, Module), S, T}}
       || {binary_field, L, {V, {integer, _, S}, {atom, _, T}}} <- Fields]}];
transform_ms({'case', _, _}, _) ->
    throw("illegal pattern");
transform_ms({call, _, _}, _) ->
    throw("illegal pattern");
transform_ms({list_comprehension, _, _}, _) ->
    throw("illegal pattern");
transform_ms({'fun', _, _}, _) ->
    throw("illegal pattern").


transform_expr({varname, _, _}=X, _) ->
    X;
transform_expr({literal, _, _}=X, _) ->
    X;

transform_expr({'fun',Line,{local,F,A}}, M) ->
    {make_fun, Line, {M,F,A}};
transform_expr({'fun',Line,{M,F,A}}, _) ->
    {make_fun, Line, {M,F,A}};
transform_expr({tuple, Line, Exprs}, Module) ->
    {call, Line,
     {{make_fun, Line,
       {{atom, Line, std},
        {atom, Line, make_tuple},
        {integer, Line, length(Exprs)}}},
      [transform_expr(Expr, Module) || Expr <- Exprs]}};
transform_expr({list, _, {[], B}}, _) ->
    B;
transform_expr({list, _, {[{_,Line,_} = H|T], B}}, Module) ->
    {call, Line,
     {{make_fun, Line,
       {{atom, Line, std},
        {atom, Line, cons},
        {integer, Line, 2}}},
     [transform_expr(H, Module),
      transform_expr({list, Line, {T,B}}, Module)]}};
transform_expr({call, Line, {var, F, A}}, M) ->
    {call, Line, {F, [transform_expr(Expr, M)||Expr<-A]}};
transform_expr({call, Line, {local, F, A}}, M) ->
    {call, Line,
     {{make_fun, Line, {{atom, Line, M}, F, {integer,Line,length(A)}}},
      [transform_expr(Expr, M)||Expr<-A]}};
transform_expr({call, Line, {M, F, A}}, Module) ->
    {call, Line,
     {{make_fun, Line, {M,F,{integer, Line, length(A)}}},
      [transform_expr(Expr, Module)||Expr<-A]}};
transform_expr({'case', Line, {Expr, Clauses}}, Module) ->
    {'case', Line,
     {[transform_expr(Expr, Module)],
      [transform_case_clause(Clause, Module) || Clause <- Clauses]}};
transform_expr({match, Line, {A, B}}, Module) ->
    MS = transform_ms(A, Module),
    case transform_expr(B, Module) of
        {'case', _, {[Expr], [{[MS1], [], []}]}} ->
            {'case', Line, {[Expr], [{[MS++MS1], [], []}]}};
        Expr ->
            {'case', Line, {[Expr], [{[MS], [], []}]}}
    end;
transform_expr({'fun', Line, Clauses}, Module) ->
    case transform_clauses(Clauses, Module) of
        {fundef, _, {{atom, _}, _, _}} ->
            throw("invalid fun");
        {fundef, _, Fun} ->
            {fundef, Line, Fun}
    end;
transform_expr({binary, Line, Fields}, Module) ->
    {binary, Line,
     [{binary_field, L, {transform_expr(V, Module), S, T}}
      || {binary_field, L, {V, {integer, _, S}, {atom, _, T}}} <- Fields,
         case {S,T} of
             {1, integer} -> true;
             {_, binary} -> true;
             _ -> false
         end]};
transform_expr({list_comprehension, _, _}, _) ->
    throw("TODO");
transform_expr({ignore, _, _}, _) ->
    throw("illegal expression").


transform_case_clause({case_clause, _, {MS, Guards, Exprs}}, Module) ->
    {[transform_ms(MS, Module)],
     [transform_expr(Expr, Module) || Expr <- Guards],
     [transform_expr(Expr, Module) || Expr <- Exprs]}.
