-module(latex).

-include("log.hrl").

-export([start_link/0, hash/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	  port :: port(),
	  callers :: dict()
	 }).

-define(TIMEOUT, 5000).

% --- api ---

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec hash(binary()) -> binary().
hash(Latex) ->
    gen_server:call(?MODULE, {hash, Latex}, ?TIMEOUT).

% --- gen_server callbacks ---

init([]) ->
    Port = open_port({spawn, "./src/latex.py"},[{packet, 2}, binary]),
    Callers = dict:new(),
    {ok, #state{port=Port, callers=Callers}}.

handle_call({hash, Latex}, From, #state{port=Port, callers=Callers}=State) ->
    Id = crypto:rand_bytes(4),
    port_command(Port, <<Id:4/binary, Latex/binary>>),
    ?INFO([send, ?VAR(Id), ?VAR(Latex)]),
    Callers2 = dict:store(Id, From, Callers),
    erlang:send_after(?TIMEOUT, self(), {timeout, Id}),
    {noreply, State#state{callers=Callers2}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, Data}}, #state{port=Port, callers=Callers}=State) ->
    <<Id:4/binary, Hash/binary>> = Data,
    ?INFO([recv, ?VAR(Id), ?VAR(Hash)]),
    case dict:find(Id, Callers) of
	{ok, From} ->
	    gen_server:reply(From, Hash);
	error ->
	    ok % too late, caller already timed out
    end,
    {noreply, State};
handle_info({timeout, Id}, #state{callers=Callers}=State) ->
    Callers2 = dict:erase(Id, Callers),
    {noreply, State#state{callers=Callers2}}.

terminate(_Reason, #state{port=Port}) ->
    try
	port_close(Port)
    catch
	error:badarg ->
	    ok
    end.

code_change(_OldVsn, State, _Extra) ->
    terminate(code_change, State),
    init([]).

% --- end ---
