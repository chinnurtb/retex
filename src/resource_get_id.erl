% REST API for requesting existing formulas, challenges and responses

-module(resource_get_id).

% webmachine callbacks
-export([init/1, allowed_methods/2, resource_exists/2, to_json/2]).
-export([content_types_accepted/2, content_types_provided/2, from_json/2]). % defined in resource.hrl

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").
-include("resource.hrl"). % magic happens in here! defines default callbacks and arg_spec()

-record(conf, {
	  module :: atom()
	 }).

-record(result, {
	  module :: atom(),
	  item :: term()
	 }).

init(Module) ->
    {ok, Module}.

allowed_methods(_ReqData, _Context) ->
    ['GET'].

resource_exists(ReqData, #conf{module=Module}=Conf) -> 
    Id = wrq:path_info(id, ReqData),
    case Module:by_id(Id) of
	{ok, Item} ->
	    {true, ReqData, #result{module=Module, item=Item}};
	{error, not_found} ->
	    {false, ReqData, Conf}
    end.

to_json(Reqdata, #result{module=Module, item=Item}=Result) ->
    Json = Module:to_json(Item),
    {mochijson2:encode(Json), Reqdata, Result}.

