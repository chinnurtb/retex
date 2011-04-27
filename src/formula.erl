-module(formula).

-include("types.hrl").

-export([start/0, read/1, new/4, random/0]).

-define(RETRIES, 10).

% autoincrementing pointers used to choose random formulas
-record(pointer, {
	  key :: integer() | 'next', % 'next' is the next id to be assigned
	  value :: id() % a formula id (this can be an integer for 'next' but dialyzer doesn't ever see it)
	 }).

-spec start() -> 'ok'.
start() ->
    ok = 
	db:ensure_table(
	  formula,
	  [ 
	    {attributes, record_info(fields, formula)},
	    {disc_copies, [node()]} 
	  ]
	 ),
    ok =
	db:ensure_table(
	  formula_all,
	  [ 
	    {attributes, record_info(fields, pointer)},
	    {disc_copies, [node()]} 
	  ]
	 ),
    % if 'next' doesnt exist this creates it with value 1, otherwise it does nothing
    _New_val = mnesia:dirty_update_counter({formula_all, next}, 1).

-spec read(id()) -> {'ok', #formula{}} | {'error', 'not_found'}.
read(Id) ->
    case mnesia:dirty_read({formula, Id}) of
	[] -> {error, not_found};
	[Formula] -> {ok, Formula}
    end.

-spec new(binary(), binary(), binary(), binary()) -> id().
new(Source, Source_id, Url, Latex) ->
    Id = id:new(formula),
    Formula = #formula{id=Id, source=Source, source_id=Source_id, url=Url, latex=Latex},
    ok = mnesia:dirty_write(Formula),
    Id.

-spec random() -> 'none' | {'ok', #formula{}}.
random() ->
    Max = mnesia:dirty_read({formula_all, next}) - 1,
    if
	Max =< 0 -> 
	    none;
	true ->
	    {atomic, Formula} =
		mnesia:transaction(
		  fun () ->
			  Choice = random:uniform(Max),
			  % this might fail if we have deleted the formula, but will get a different choice on retry
			  [Id] = mnesia:dirty_read({formula_all, Choice}),
			  {ok, Formula} = mnesia:read({formula, Id}), 
			  Formula
		  end,
		  ?RETRIES
		 ),
	    {ok, Formula}
    end.
		  
