-module(test).

-include("types.hrl").
-include_lib("proper/include/proper.hrl").

-export([test/1]).

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

printable() ->
    ?LET(String, string(), mochijson2:decode(mochijson2:encode(String))).

id_type() ->
    oneof([formula, challenge, response]).

alphanumeric() ->
    oneof("abcdefghijklmnopqrstuvwxyz0123456789").

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
    lists:flatten(path_to_url(Path, [])).
path_to_url([], Acc) ->
    ["http://localhost:8000" | Acc];
path_to_url([Elem|Path], Acc) ->
    path_to_url(Path, [Acc, "/", Elem]).
	      
http_post(Path, Args) ->
    Body = mochijson2:encode({struct, Args}),
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

post_challenge(Source) ->
    http_post(["challenge"], [{<<"source">>, Source}]).

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

prop_post_challenges() ->
    ?FORALL(Formula_args, non_empty(list(formula_arg())),
	    ?FORALL(Sources, list(printable()),
		    begin
			clean(),
			lists:foreach(
			  fun (Formula_arg) ->
				  apply(formula, new, Formula_arg)
			  end,
			  Formula_args
			 ),
			lists:foreach(
			  fun (Source) ->
				  {201, Json} = post_challenge(Source),
				  [Id, Formulas] = json_gets(Json, [id, formulas]),
				  {200, Json} = get_challenge(Id),
				  lists:foreach(
				    fun (Formula) ->
					    {200, _Json2} = get_formula(Formula)
				    end,
				    Formulas
				   )
			  end,
			  Sources
			 ),
			true
		    end
		   )
	    ).
		

% --- api ---

test(N) ->
    proper:module(test, {numtests, N}).

% --- end ---
