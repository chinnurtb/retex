-module(formula).

-include("types.hrl").

-export([start/0, get/1, new/4]).

start() ->
    ok = 
	db:ensure_table(
	  formula,
	  [ 
	    {attributes, record_info(fields, formula)},
	    {disc_copies, [node()]} 
	  ]
	 ).

get(Id) ->
    case mnesia:dirty_read({formula, Id}) of
	[] -> {error, not_found};
	[Formula] -> {ok, Formula}
    end.

new(Source, Source_id, Url, Latex) ->
    Id = id:new(formula),
    Formula = #formula{id=Id, source=Source, source_id=Source_id, url=Url, latex=Latex},
    ok = mnesia:dirty_write(Formula),
    Id.
