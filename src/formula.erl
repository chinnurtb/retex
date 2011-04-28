-module(formula).

-include("types.hrl").

-export([start/0, read/1, new/4, random/0]).

-define(RETRIES, 10).

-spec start() -> ok.
start() ->
    ok = 
	db:ensure_table(
	  formula,
	  [ 
	    {attributes, record_info(fields, formula)},
	    {disc_copies, [node()]} 
	  ]
	 ),
    % this table maps autoincrementing ids to formula ids, so we can pick formulas at random
    ok = autoinc:start(formula_all).

-spec read(id()) -> {ok, #formula{}} | {error, not_found}.
read(Id) ->
    case mnesia:dirty_read({formula, Id}) of
	[] -> {error, not_found};
	[Formula] -> {ok, Formula}
    end.

-spec new(binary(), binary(), binary(), binary()) -> #formula{}.
new(Source, Source_id, Url, Latex) ->
    Id = id:new(formula),
    Formula = #formula{id=Id, source=Source, source_id=Source_id, url=Url, latex=Latex},
    {atomic, _} = 
	mnesia:transaction(
	  fun () ->
		  ok = mnesia:write(Formula),
		  autoinc:put(formula_all, Id)
	  end,
	  ?RETRIES
	 ),
    Formula.

-spec random() -> #formula{}.
random() ->
    {atomic, Formula} =
	mnesia:transaction(
	  fun () ->
		  {ok, Id} = autoinc:random(formula_all),
		  % this might fail if we have deleted the formula, but will get a different choice on retry
		  {ok, Formula} = read(Id), 
		  Formula
	  end,
	  ?RETRIES
	 ),
    Formula.
