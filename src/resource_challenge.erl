% REST API for creating new challenges

-module(resource_challenge).

% webmachine callbacks
-export([init/1, allowed_methods/2, post_is_create/2, create_path/2, content_types_accepted/2, from_json/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").

init([]) ->
    {{trace, "/tmp"}, none}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    Id = id:new(challenge),
    {binary_to_list(Id), ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", from_json}], ReqData, Context}.

from_json(ReqData, _Context) ->
    Id = list_to_binary(wrq:disp_path(ReqData)),
    #formula{id=Formula_id} = formula:random(),
    case json:decode_request_body(ReqData, [source]) of
	{ok, [Source]} when is_binary(Source) ->
	    Challenge = challenge:new(Id, {single, Source}, [Formula_id]),
	    {true, ReqData, Challenge};
	{error, _Error} ->
	    {halt, 400} % bad request
    end.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

to_json(ReqData, Challenge) ->
    Json = challenge:to_json(Challenge),
    {mochijson2:encode(Json), ReqData, Challenge}.
