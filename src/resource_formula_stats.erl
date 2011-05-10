% REST API for retrieving stats about a given formula

-module(resource_formula_stats).

% webmachine callbacks
-export([init/1, allowed_methods/2, resource_exists/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").

init([]) ->
    {ok, none}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

resource_exists(ReqData, Context) -> 
    Id = list_to_binary(wrq:path_info(id, ReqData)),
    case formula:by_id(Id) of
	{ok, _} ->
	    Responses = response:by_formula_id(Id),
	    {true, ReqData, Responses};
	{error, not_found} ->
	    {false, ReqData, Context}
    end.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

to_json(Reqdata, Responses) ->
    Json = 
	{struct,
	 [
	  {<<"responses">>, {struct, group_by_hash(Responses)}}
	 ]
	},
    {mochijson2:encode(Json), Reqdata, Responses}.

% --- internal functions ---

-spec group_by_hash(list(#response{})) -> list({Latex :: binary(), Num :: integer()}).
group_by_hash(Responses) ->
    Pairs = lists:flatten([lists:zip(Latexs, Hashes) || #response{latexs=Latexs, hashes=Hashes} <- Responses]),
    Dict = 
	lists:foldl(
	  fun ({Latex, Hash}, Dict_acc) ->
		  dict:update(Hash, fun ({_, Num}) -> {Latex, Num+1} end, {Latex, 1}, Dict_acc)
	  end,
	  dict:new(),
	  Pairs
	 ),
    [Stats || {_Hash, Stats} <- dict:to_list(Dict)].

