-module(retex_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    crypto:start(),
    db:ensure_schema(),
    mnesia:start(),
    formula:start(),
    challenge:start(),
    response:start(),
    retex_sup:start_link().

stop(_State) ->
    mnesia:stop(),
    crypto:stop(),
    ok.
