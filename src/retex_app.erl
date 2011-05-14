%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the retex application.

-module(retex_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).

-include("log.hrl").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for retex.
start(_Type, _StartArgs) ->
    ?INFO([retex, starting]),
    ok = formula:start(),
    ok = challenge:start(),
    ok = response:start(),
    retex_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for retex.
stop(_State) ->
    ?INFO([retex, stopping]),
    ok.
