% REST API for requesting existing formulas, challenges and responses

-module(resource_get_id).

% webmachine callbacks
-export([init/1, allowed_methods/2, resource_exists/2,  content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").

-record(conf, {
	  module :: atom()
	 }).

-record(result, {
	  module :: atom(),
	  item :: term()
	 }).

init(Module) ->
    {ok, #conf{module=Module}}.

allowed_methods(ReqData, Context) ->
    {['GET'], ReqData, Context}.

resource_exists(ReqData, #conf{module=Module}=Conf) -> 
    Id = list_to_binary(wrq:path_info(id, ReqData)),
    case Module:by_id(Id) of
	{ok, Item} ->
	    {true, ReqData, #result{module=Module, item=Item}};
	{error, not_found} ->
	    {false, ReqData, Conf}
    end.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

to_json(Reqdata, #result{module=Module, item=Item}=Result) ->
    Json = Module:to_json(Item),
    {mochijson2:encode(Json), Reqdata, Result}.

