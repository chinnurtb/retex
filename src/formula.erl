-module(formula).

-include("types.hrl").

-export([start/0, read/1, new/4]).

-spec start() -> 'ok'.
start() ->
    ok = 
	db:ensure_table(
	  formula,
	  [ 
	    {attributes, record_info(fields, formula)},
	    {disc_copies, [node()]} 
	  ]
	 ).

-spec read(id()) -> {'ok', #formula{}} | {'error', 'not_found'}.
read(Id) ->
    case mnesia:dirty_read({formula, Id}) of
	[] -> {error, not_found};
	[Formula] -> {ok, Formula}
    end.

-spec new(binary(), binary(), binary(), binary()) -> id().
new(Source, Source_id, Url, Latex) ->
    Id = id:new(formula),
    Formula = #formula{id=Id, source=Source, source_id=Source_id, url=Url, latex=Latex},
    ok = mnesia:dirty_write(Formula),
    Id.
