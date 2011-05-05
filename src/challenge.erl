-module(challenge).

-include("types.hrl").

-export([start/0, by_id/1, new/2, timeout/1, responded/1, to_json/1]).

-define(TIMEOUT, 1000*60*5). % 5 minutes

-spec start() -> ok.
start() ->
    ok = 
	db:ensure_table(
	  challenge,
	  [ 
	    {attributes, record_info(fields, challenge)},
	    {ram_copies, [node()]} 
	  ]
	 ).

-spec by_id(id()) -> {ok, #challenge{}} | {error, not_found}.
by_id(Id) ->
    case mnesia:dirty_read({challenge, Id}) of
	[] -> {error, not_found};
	[Challenge] -> {ok, Challenge}
    end.

-spec new(term(), list(id())) -> #challenge{}.
new(Source, Formulas) ->
    Id = id:new(challenge),
    Challenge = #challenge{id=Id, generated=now(), source=Source, formulas=Formulas},
    {ok, _} = timer:apply_after(?TIMEOUT, challenge, timeout, [Id]),
    ok = mnesia:dirty_write(Challenge),
    Challenge.

-spec timeout(id()) -> ok.
timeout(Id) ->
    ok = mnesia:dirty_delete({challenge, Id}). 

-spec responded(id()) -> ok.
responded(Id) ->
    ok = mnesia:dirty_delete({challenge, Id}). 

-spec to_json(#challenge{}) -> json:json_object().
to_json(#challenge{id=Id, formulas=Formulas}) ->
    {struct, 
     [
      {<<"id">>, Id},
      {<<"formulas">>, Formulas}
     ]
    }.
