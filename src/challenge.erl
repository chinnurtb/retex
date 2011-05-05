-module(challenge).

-include("types.hrl").
-include("util.hrl").

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
    case ?TRANS(mnesia:read({challenge, Id})) of
	{atomic, []} -> {error, not_found};
	{atomic, [Challenge]} -> {ok, Challenge}
    end.

-spec new(term(), list(id())) -> #challenge{}.
new(Source, Formulas) ->
    Id = id:new(challenge),
    Challenge = #challenge{id=Id, generated=now(), source=Source, formulas=Formulas},
    {ok, _} = timer:apply_after(?TIMEOUT, challenge, timeout, [Id]),
    {atomic, ok} = ?TRANS(mnesia:write(Challenge)),
    Challenge.

-spec timeout(id()) -> ok.
timeout(Id) ->
    ok = mnesia:dirty_delete({challenge, Id}). 

-spec responded(id()) -> ok.
responded(Id) ->
    {atomic, ok} = ?TRANS(mnesia:delete({challenge, Id})),
    ok. 

-spec to_json(#challenge{}) -> json:json_object().
to_json(#challenge{id=Id, formulas=Formulas}) ->
    {struct, 
     [
      {<<"id">>, Id},
      {<<"formulas">>, Formulas}
     ]
    }.
