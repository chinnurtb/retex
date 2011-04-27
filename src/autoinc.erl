% autoincrementing pointers into other tables
-module(autoinc).

-include("types.hrl").

-export([start/1, put/2, get/2, random/1]).

-define(RETRIES, 10).

-type key() :: integer() | next. % 'next' is the next id to be assigned
-type value() :: id() | integer(). % the value pointed to (or next key if key is 'next'

-record(pointer, {
	  key :: key(), 
	  value :: value () 
	 }).

-spec read(atom(), key()) -> value().
read(Table, Key) ->
    [#pointer{value=Value}] = mnesia:read({Table, Key}),
    Value.

-spec write(atom(), key(), value()) -> 'ok'.
write(Table, Key, Value) ->
    ok = mnesia:write(Table, #pointer{key=Key, value=Value}, write).

-spec start(atom()) -> 'ok'.
start(Table) ->
    ok = 
	db:ensure_table(
	  Table,
	  [ 
	    {record_name, pointer},
	    {attributes, record_info(fields, pointer)},
	    {disc_copies, [node()]} 
	  ]
	 ),
    {atomic, ok} =
	mnesia:transaction(
	  fun () ->
		  case mnesia:read({Table, next}) of
		      [] ->
			  ok = write(Table, next, 0);
		      _ ->
			  ok
		  end
	  end,
	  ?RETRIES
	 ),
    ok.

-spec put(atom(), id()) -> integer(). 
put(Table, Id) ->
    {atomic, Next} =
	mnesia:transaction(
	  fun () ->
		  Next = read(Table, next),
		  ok = write(Table, next, Next+1),
		  ok = write(Table, Next, Id),
		  Next
	  end,
	  ?RETRIES
	 ),
    Next.

-spec get(atom(), integer()) -> {'error', 'not_found'} | {'ok', id()}.
get(Table, Key) ->
    case mnesia:dirty_read({Table, Key}) of
	[] -> {error, not_found};
	[#pointer{value=Value}] -> {ok, Value}
    end.

-spec random(atom()) -> 'none' | {'ok', id()}.
random(Table) ->
    Next = read(Table, next),
    if 
	Next =< 0 ->
	    none;
	true ->
	    {atomic, Id} =
		mnesia:transaction(
		  fun () ->
			  Choice = random:uniform(Next) - 1,
			  read(Table, Choice)
		  end,
		  ?RETRIES
		 ),
	    {ok, Id}
    end.
