-module(challenge).

-include("types.hrl").
-include("util.hrl").
-include("log.hrl").

-export([start/0, by_id/1, new/2, new/3, timeout/1, responded/1, to_json/1]).

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
    new(Id, Source, Formulas).

-spec new(id(), term(), list(id())) -> #challenge{}.
new(Id, Source, Formulas) ->
    ?INFO([new, ?VAR(Id), ?VAR(Source), ?VAR(Formulas)]),
    Challenge = #challenge{id=Id, generated=now(), source=Source, formulas=Formulas},
    {ok, _} = timer:apply_after(?TIMEOUT, challenge, timeout, [Id]),
    {atomic, ok} = ?TRANS(mnesia:write(Challenge)),
    Challenge.

-spec timeout(id()) -> ok.
timeout(Id) ->
    ok = mnesia:dirty_delete({challenge, Id}). 

-spec responded(id()) -> ok.
responded(Id) ->
    ?INFO([responded, ?VAR(Id)]),
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
