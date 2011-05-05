% REST API for retrieving stats about a given formula

-module(resource_formula_stats).

% webmachine callbacks
-export([init/1, allowed_methods/2, resource_exists/2, to_json/2]).
-export([content_types_accepted/2, content_types_provided/2, from_json/2]). % defined in resource.hrl

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").
-include("resource.hrl"). % magic happens in here! defines default callbacks and arg_spec()

-record(result, {
	  responses :: list(#response{})
	 }).

init([]) ->
    {ok, none}.

allowed_methods(_ReqData, _Context) ->
    ['GET'].

resource_exists(ReqData, Context) -> 
    Id = wrq:path_info(id, ReqData),
    case formula:by_id(Id) of
	{ok, _} ->
	    Responses = response:by_formula_id(Id),
	    {true, ReqData, #result{responses=Responses}};
	{error, not_found} ->
	    {false, ReqData, Context}
    end.

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
    dict:to_list(Dict).

to_json(Reqdata, #result{responses=Responses}=Result) ->
    Json = 
	{struct,
	 [
	  {<<"responses">>, group_by_hash(Responses)}
	 ]
	},
    {mochijson2:encode(Json), Reqdata, Result}.

