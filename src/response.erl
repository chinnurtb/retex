-module(response).

-include("types.hrl").
-include("util.hrl").

-export([start/0, by_id/1, by_formula_id/1, new/3, new/4, to_json/1]).

-spec start() -> ok.
start() ->
    ok = 
	db:ensure_table(
	  response,
	  [ 
	    {attributes, record_info(fields, response)},
	    {disc_copies, [node()]},
	    {index, [formulas]}
	  ]
	 ).

-spec by_id(id()) -> {ok, #response{}} | {error, not_found}.
by_id(Id) ->
    case ?TRANS(mnesia:read({response, Id})) of
	{atomic, []} -> {error, not_found};
	{atomic, [Response]} -> {ok, Response}
    end.

-spec by_formula_id(id()) -> list(#response{}).
by_formula_id(Formula_id) ->
    {atomic, Responses} = ?TRANS(mnesia:index_match_object(#response{formulas=[Formula_id], _ ='_'}, formulas)),
    Responses.

-spec new(id(), binary(), list(binary())) -> #response{}.
new(Challenge_id, User_id, Latexs) ->
    Id = id:new(response),
    new(Id, Challenge_id, User_id, Latexs).

-spec new(id(), id(), binary(), list(binary())) -> #response{}.
new(Id, Challenge_id, User_id, Latexs) ->
    {atomic, Response} =
	mnesia:transaction(
	  fun () ->
		  {ok, #challenge{
		     generated=Generated, 
		     source=Source, 
		     formulas=Formulas}} = challenge:by_id(Challenge_id),
		  Response = #response{
		    id=Id,
		    generated=Generated,
		    submitted=now(),
		    source=Source,
		    user_id=User_id,
		    formulas=Formulas,
		    latexs=Latexs,
		    hashes=lists:map(fun latex:hash/1, Latexs)
		   },
		  ok = mnesia:write(Response),
		  ok = challenge:responded(Challenge_id),
		  Response
	  end,
	  ?RETRIES
	 ),
    Response.

-spec to_json(#response{}) -> json:json_object().
to_json(#response{id=Id}) ->
    {struct,
     [
      {<<"id">>, Id}
     ]
    }.
