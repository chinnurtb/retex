% REST API for responding to challenges

-module(resource_response).

% webmachine callbacks
-export([init/1, allowed_methods/2, post_is_create/2, create_path/2, content_types_accepted/2, from_json/2, content_types_provided/2, to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("types.hrl").

init([]) ->
    {ok, none}.

allowed_methods(ReqData, Context) ->
    {['POST'], ReqData, Context}.

post_is_create(ReqData, Context) ->
    {true, ReqData, Context}.

create_path(ReqData, Context) ->
    Id = id:new(response),
    {binary_to_list(Id), ReqData, Context}.

content_types_accepted(ReqData, Context) ->
   {[{"application/json", from_json}], ReqData, Context}.

from_json(ReqData, _Context) ->
    Id = list_to_binary(wrq:disp_path(ReqData)),
    case json:decode_request_body(ReqData, [challenge, user, {latexs, fun is_latexs/1}]) of
	{ok, [Challenge, User, Latexs]} when is_binary(Challenge), is_binary(User) ->
	    Response = response:new(Id, Challenge, User, Latexs),
	    {true, ReqData, Response};
	{error, _Error} ->
	    {halt, 400} % bad request
    end.

content_types_provided(ReqData, Context) ->
   {[{"application/json", to_json}], ReqData, Context}.

to_json(Reqdata, Response) ->
    Json = response:to_json(Response),
    {mochijson2:encode(Json), Reqdata, Response}.

% --- internal functions ---

is_latexs(Latexs) when is_list(Latexs) ->
    true = lists:all(fun is_binary/1, Latexs).
