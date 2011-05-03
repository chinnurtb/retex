% REST API for responding to challenges

-module(resource_response).

% webmachine callbacks
-export([init/1, allowed_methods/2, post_is_create/2, create_path/2, to_json/2]).
-export([content_types_accepted/2, content_types_provided/2, from_json/2]). % defined in resource.hrl

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").
-include("resource.hrl"). % magic happens in here! defines default callbacks and arg_spec()

-record(request, {
	  challenge :: id(),
	  user :: binary(),
	  latexs :: list(binary())
	 }).

-record(result, {
	  response :: #response{}
	  }).

init([]) ->
    Arg_spec = 
	{arg_spec, request, 
	 [
	  {challenge, fun (Json) when is_binary(Json) -> Json end},
	  {user, fun (Json) when is_binary(Json) -> Json end},
	  {latexs, fun (Json) when is_list(Json) -> true = lists:all(fun (Latex) -> is_binary(Latex) end, Json) end}
	 ]
	},
    {ok, Arg_spec}.

allowed_methods(_ReqData, _Context) ->
    ['POST'].

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, #request{challenge=Challenge, user=User, latexs=Latexs}) ->
    #response{id = Id} = Response = response:new(Challenge, User, Latexs),
    Result = #result{response=Response},
    {Id, ReqData, Result}.

to_json(Reqdata, #result{response=Response}=Result) ->
    Json = response:to_json(Response),
    {mochijson2:encode(Json), Reqdata, Result}.
