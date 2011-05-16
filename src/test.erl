-module(test).

-include("types.hrl").
-include_lib("proper/include/proper.hrl").

-include("basho_bench.hrl").
-export([new/1, run/4]).

-export([clean/0, generate_corpus/1, test/1, bench/0]).

-define(HOST, "http://localhost:8000").

-define(MATCHES(Pattern, Value),
	begin
	    V = Value,
	    try
		Pattern = V,
		true
	    catch
		error:{badmatch, V} ->
		    io:format("~p", [{badmatch, ??Pattern, V}]),
		    false
	    end
	end
       ).

% --- generators ---

alphanumeric() ->
    oneof("abcdefghijklmnopqrstuvwxyz0123456789").

printable() ->
    % !!! want to do proper unicode here
    ?LET(String, list(alphanumeric()), list_to_binary(String)).

id_type() ->
    oneof([formula, challenge, response]).

id() ->
    ?LET({Type, Suffix}, {id_type(), vector(28, alphanumeric())},
	 begin
	     Prefix = 
		 case Type of
		     formula -> <<"form">>;
		     challenge -> <<"chal">>;
		     response -> <<"resp">>
		 end,
	     << Prefix/binary, Suffix >>
	 end).

latex() ->
    % !!! need a better gen, maybe read from file
    oneof(
      [ 
       <<"\$ 1+1=2 \$">>,
       <<"\$\$ e ^ {2 * \pi * i} \$\$">>
      ]
     ).

% --- util functions ---

tables() ->
    [formula, formula_all, challenge, response].

clean_table(Table) ->
    lists:foreach(
      fun (Key) -> 
	      case Key of 
		  next -> mnesia:dirty_write(Table, {pointer, Key, 0});
		  _ -> mnesia:dirty_delete(Table, Key) 
	      end
      end, 
      mnesia:dirty_all_keys(Table)
     ).

clean() ->
    lists:foreach(fun clean_table/1, tables()),
    ok.

path_to_url(Path) ->
    lists:flatten([?HOST, path_to_url(Path, [])]).
path_to_url([], Acc) ->
    Acc;
path_to_url([Elem|Path], Acc) ->
    path_to_url(Path, [Acc, "/", Elem]).
	      
http_post(Path, Args) ->
    Body = iolist_to_binary(mochijson2:encode({struct, Args})),
    {ok, {Code, Result}} = 
	httpc:request(
	  post, 
	  {path_to_url(Path), [], "application/json", Body}, 
	  [], 
	  [{full_result, false}]
	 ),
    {Code, mochijson2:decode(Result)}.

http_get(Path) ->
    {ok, {Code, Result}} = 
	httpc:request(
	  get, 
	  {path_to_url(Path), []}, 
	  [], 
	  [{full_result, false}]
	 ),
    {Code, mochijson2:decode(Result)}.

get_formula(Id) ->
    http_get(["formula", binary_to_list(Id)]).

get_challenge(Id) ->
    http_get(["challenge", binary_to_list(Id)]).

get_response(Id) ->
    http_get(["response", binary_to_list(Id)]).

get_formula_stats(Id) ->
    http_get(["formula", binary_to_list(Id), "stats"]).

post_challenge(Source) ->
    http_post(["challenge"], [{<<"source">>, Source}]).

post_response(Challenge_id, User, Latexs) ->
    http_post(["response"], [{<<"challenge">>, Challenge_id}, {<<"user">>, User}, {<<"latexs">>, Latexs}]).

json_gets(Json, Paths) ->
    lists:map(
      fun (Path) ->
	      {ok, Value} = json:get(Json, Path),
	      Value
      end,
      Paths
     ).

% --- properties testing internal functions ---

formula_arg() ->
    [printable(), printable(), printable(), latex()]. 

prop_create_formulas() ->
    ?FORALL(Args, list(formula_arg()),
	    begin
		clean(),
		lists:all(
		  fun (Arg) ->
			  #formula{id=Id} = Formula = apply(formula, new, Arg),
			  formula:by_id(Id) == {ok, Formula}
		  end, 
		  Args
		 )
	    end
	   ).
		
prop_random_formula_single() ->
    ?FORALL(Arg, formula_arg(),
	    begin
		clean(),
		Formula = apply(formula, new, Arg),
		equals(formula:random(), Formula)
	    end
	   ).

% --- properties testing rest api ---

prop_get_formula() ->
    ?FORALL(Arg, formula_arg(),
	    begin
		clean(),
		#formula{id=Id, url=Url, latex=Latex} = apply(formula, new, Arg),
		{Code, Json} = get_formula(Id),
		equals(
		  {Code, json_gets(Json, [id, url, latex])},
		  {200, [Id, Url, Latex]}
		 )
	    end
	   ).

create_formulas(Formula_args) ->
    lists:foreach(
      fun (Formula_arg) ->
	      apply(formula, new, Formula_arg)
      end,
      Formula_args
     ).

prop_post_challenge() ->
    ?FORALL(Formula_args, non_empty(list(formula_arg())),
	    ?FORALL(Source, printable(),
		    begin
			clean(),
			create_formulas(Formula_args),
			{201, Json} = post_challenge(Source),
			[Id, Formulas] = json_gets(Json, [id, formulas]),
			{200, Json} = get_challenge(Id),
			lists:foreach(
			  fun (Formula) ->
				  {200, _Json2} = get_formula(Formula)
			  end,
			  Formulas
			 ),
			true
		    end
		   )
	    ).

challenge_response(Source, User, Latexs) ->
    {201, Json} = post_challenge(Source),
    [Challenge_id, Formulas] = json_gets(Json, [id, formulas]),
    {201, Json2} = post_response(Challenge_id, User, Latexs),
    {Formulas, Json2}.

prop_post_response() ->
    ?FORALL(Formula_args, non_empty(list(formula_arg())),
	    ?FORALL(Source, printable(),
		    ?FORALL([User, Latexs], [printable(), vector(1, latex())],
			    begin
				clean(),
				create_formulas(Formula_args),
				{_, Json} = challenge_response(Source, User, Latexs),
				{ok, Id} = json:get(Json, id),
				{200, Json} = get_response(Id),
				true
			    end
			   )
		   )
	   ).

check_stats(Formulas, Latexs, Number) ->
    lists:foreach(
      fun ({Formula, Latex}) ->
	      {200, Json} = get_formula_stats(Formula),
	      {ok, Number} = json:get(Json, [responses, Latex])
      end,
      lists:zip(Formulas, Latexs)
     ).

prop_get_formula_stats() ->
    ?FORALL(Formula_args, non_empty(list(formula_arg())),
	    ?FORALL(Source, printable(),
		    ?FORALL([User, Latexs], [printable(), vector(1, latex())],
			    begin
				clean(),
				create_formulas(Formula_args),
				{Formulas, _} = challenge_response(Source, User, Latexs),
				check_stats(Formulas, Latexs, 1),
				true
			    end
			   )
		   )
	   ).

% --- basho bench driver ---

pick(Gen, Size) ->
    proper_gen:pick(Gen, Size).

generate_corpus(N) ->
    lists:foreach(
      fun (_) ->
	      {ok, Formula_arg} = pick(formula_arg(), 1000),
	      apply(formula, new, Formula_arg)
      end,
      lists:seq(1,N)
     ).

new(_Id) ->
    {ok, none}.

run(single, KeyGen, _ValueGen, State) -> 
    Size = KeyGen(),
    {ok, Source} = pick(printable(), Size),
    {ok, User} = pick(printable(), Size),
    {ok, Latexs} = pick(vector(1, latex()), Size),
    try
	{Formulas, _} = challenge_response(Source, User, Latexs),
	lists:foreach(
	  fun ({Formula, _Latex}) ->
		  {200, _Json} = get_formula_stats(Formula)
	  end,
	  lists:zip(Formulas, Latexs)
	 ),
	{ok, State}
    catch
	Class:Error ->
 	    {error, {Class, Error, erlang:get_stacktrace()}, State}
    end.

% --- api ---

test(N) ->
    inets:start(),
    proper:module(test, {numtests, N}).

bench() ->
    inets:start(),
    basho_bench:main(["src/bench.config"]).

% --- end ---
