-module(latex).

-export([start_link/0, hash/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	  port :: port()
	 }).

-define(TIMEOUT, 1000).

% --- api ---

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

hash(Latex) ->
    gen_server:call(?MODULE, {hash, Latex}, ?TIMEOUT).

% --- gen_server callbacks ---

init([]) ->
    Port = open_port({spawn, "./src/latex.py"},[{packet, 2}, binary]),
    {ok, #state{port=Port}}.

handle_call({hash, Latex}, _From, #state{port=Port}=State) ->
    port_command(Port, Latex),
    receive
	{Port, {data, Data}} -> 
	    {reply, Data, State}
    after 
	?TIMEOUT -> 
	    {noreply, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

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
