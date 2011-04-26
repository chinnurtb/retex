-module(response).

-include("types.hrl").

-export([start/0, get/1, new/3]).

-define(RETRIES, 10).

start() ->
    ok = 
	db:ensure_table(
	  response,
	  [ 
	    {attributes, record_info(fields, response)},
	    {disc_copies, [node()]} 
	  ]
	 ).

get(Id) ->
    case mnesia:dirty_read({response, Id}) of
	[] -> {error, not_found};
	[Response] -> {ok, Response}
    end.

new(Challenge_id, User_id, Latexs) ->
    Id = id:new(response),
    {atomic, _} =
	mnesia:transaction(
	  fun () ->
		  {ok, #challenge{
		     generated=Generated, 
		     source=Source, 
		     formulae=Formulae}} = challenge:get(Challenge_id),
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
		  challenge:responded(Challenge_id)
	  end,
	  ?RETRIES
	 ),
    Id.
