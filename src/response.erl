-module(response).

-include("types.hrl").

-export([start/0, read/1, new/3]).

-define(RETRIES, 10).

-spec start() -> 'ok'.
start() ->
    ok = 
	db:ensure_table(
	  response,
	  [ 
	    {attributes, record_info(fields, response)},
	    {disc_copies, [node()]} 
	  ]
	 ).

-spec read(id()) -> {'ok', #response{}} | {'error', 'not_found'}.
read(Id) ->
    case mnesia:dirty_read({response, Id}) of
	[] -> {error, not_found};
	[Response] -> {ok, Response}
    end.

-spec new(id(), binary(), list(binary())) -> id().
new(Challenge_id, User_id, Latexs) ->
    Id = id:new(response),
    {atomic, _} =
	mnesia:transaction(
	  fun () ->
		  {ok, #challenge{
		     generated=Generated, 
		     source=Source, 
		     formulae=Formulae}} = challenge:read(Challenge_id),
		  Response = #response{
		    id=Id,
		    generated=Generated,
		    submitted=now(),
		    source=Source,
		    user_id=User_id,
		    formulae=Formulae,
		    latexs=Latexs,
		    hashes=lists:map(fun latex:hash/1, Latexs)
		   },
		  ok = mnesia:write(Response),
		  ok = challenge:responded(Challenge_id)
	  end,
	  ?RETRIES
	 ),
    Id.
