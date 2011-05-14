-module(db).

-export([ensure_schema/0, ensure_table/2, backup/1, restore/1]).

-include("log.hrl").

-spec ensure_schema() -> ok | {error, Reason :: term()}.
ensure_schema() ->
    case mnesia:create_schema([node()]) of
	ok ->
	    ok;
	{error, {_Host, {already_exists, _}}} ->
	    ok;
	{error, {_Host, Error}} ->
	    {error, Error}
    end.

-spec ensure_table(atom(), Tab_def :: list({atom(), term()})) ->  ok | {error, Reason :: term()}.
ensure_table(Name, Tab_def) ->
    case mnesia:create_table(Name, Tab_def) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, Name}} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.

-spec backup(string()) -> ok | {error, Reason :: term()}.
backup(Filename) ->
    Result = mnesia:backup(Filename),
    ?INFO([backup, ?VAR(Filename), ?VAR(Result)]),
    Result.

-spec restore(string()) -> {atomic, Tables :: list(atom())} | {aborted, Reason :: term()}.
restore(Filename) ->
    Result = mnesia:restore(Filename, [{default_op, recreate_tables}]),
    ?INFO([restore, ?VAR(Filename), ?VAR(Result)]),
    Result.
    
