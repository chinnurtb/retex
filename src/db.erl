-module(db).

-export([start/0, ensure_schema/0, ensure_table/2]).

start() ->
    ok = mnesia:start().

ensure_schema() ->
    case mnesia:create_schema([node()]) of
	ok ->
	    ok;
	{error, {_Host, {already_exists, _}}} ->
	    ok;
	{error, {_Host, Error}} ->
	    {error, Error}
    end.

ensure_table(Name, Tab_def) ->
    case mnesia:create_table(Name, Tab_def) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, Name}} ->
	    ok;
	{aborted, Reason} ->
	    {error, Reason}
    end.
