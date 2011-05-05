% REST API for creating new challenges

-module(resource_challenge).

% webmachine callbacks
-export([init/1, allowed_methods/2, post_is_create/2, create_path/2, to_json/2]).
-export([content_types_accepted/2, content_types_provided/2, from_json/2]). % defined in resource.hrl

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").
-include("resource.hrl"). % magic happens in here! defines default callbacks and arg_spec()

-record(request, {
	  source :: binary()
	 }).

-record(result, {
	  challenge :: #challenge{}
	  }).

init([]) ->
    Arg_spec = 
	{arg_spec, request, 
	 [
	  {source, fun (Json) when is_binary(Json) -> Json end}
	 ]
	},
    {{trace, "/tmp"}, Arg_spec}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, #request{source=Source}) ->
    #formula{id=Formula_id} = formula:random(),
    #challenge{id=Id} = Challenge = challenge:new({single, Source}, [Formula_id]),
    Result = #result{challenge=Challenge},
    {binary_to_list(Id), ReqData, Result}.

to_json(Reqdata, #result{challenge=Challenge}=Result) ->
    Json = challenge:to_json(Challenge),
    {mochijson2:encode(Json), Reqdata, Result}.
