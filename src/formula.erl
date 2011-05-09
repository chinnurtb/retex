
-module(formula).

-include("types.hrl").
-include("util.hrl").

-export([start/0, by_id/1, new/4, new/5, random/0, to_json/1]).

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

-spec by_id(id()) -> {ok, #formula{}} | {error, not_found}.
by_id(Id) ->
    case ?TRANS(mnesia:read({formula, Id})) of
	{atomic, []} -> {error, not_found};
	{atomic, [Formula]} -> {ok, Formula}
    end.

-spec new(binary(), binary(), binary(), binary()) -> #formula{}.
new(Source, Source_id, Url, Latex) ->
    Id = id:new(formula),
    new(Id, Source, Source_id, Url, Latex).

-spec new(id(), binary(), binary(), binary(), binary()) -> #formula{}.
new(Id, Source, Source_id, Url, Latex) ->
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
		  {ok, Formula} = by_id(Id), 
		  Formula
	  end,
	  ?RETRIES
	 ),
    Formula.

-spec to_json(#formula{}) -> json:json_obj().
to_json(#formula{id=Id, url=Url, latex=Latex}) ->
    {struct,
     [
      {<<"id">>, Id},
      {<<"url">>, Url},
      {<<"latex">>, Latex}
     ]
    }.
