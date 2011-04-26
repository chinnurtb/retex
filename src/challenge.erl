-module(challenge).

-include("types.hrl").

-export([start/0, get/1, new/2, timeout/1, responded/1]).

-define(TIMEOUT, 1000*60*5). % 5 minutes

start() ->
    ok = 
	db:ensure_table(
	  challenge,
	  [ 
	    {attributes, record_info(fields, challenge)},
	    {ram_copies, [node()]} 
	  ]
	 ).

get(Id) ->
    case mnesia:dirty_read({challenge, Id}) of
	[] -> {error, not_found};
	[Challenge] -> {ok, Challenge}
    end.

new(Source, Formulae) ->
    Id = id:new(challenge),
    Challenge = #challenge{id=Id, generated=now(), source=Source, formulae=Formulae},
    {ok, _} = timer:apply_after(?TIMEOUT, challenge, timeout, [Id]),
    ok = mnesia:dirty_write(Challenge),
    Id.

timeout(Id) ->
    ok = mnesia:dirty_delete({challenge, Id}). 

responded(Id) ->
    ok = mnesia:dirty_delete({challenge, Id}). 
