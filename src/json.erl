% utils for working with json (as encoded by mochijson2)

-module(json).

-export([get/2, set/3, update/4]).
-export([decode_request_body/2]).

-type json_string() :: atom | binary().
-type json_number() :: integer() | float().
-type json_array() :: [json()].
-type json_object() :: {struct, [{json_string(), json()}]}.
-type json() :: json_string() | json_number() | 
		json_array() | json_object().

-export_type([json_string/0, json_number/0, json_array/0, json_object/0, json/0]).

% json accessors

-type path() :: json_string() | integer() | list(path()).

-export_type([path/0]).

-spec get(json(), path()) -> {ok, json()} | {error, not_found}.
get(Json, Path) when is_atom(Path) ->
    get(Json, list_to_binary(atom_to_list(Path)));
get({struct, Json}, Path) when is_list(Json) and is_binary(Path) ->
    case lists:keyfind(Path, 1, Json) of
	false -> {error, not_found};
	{Path, Value} -> {ok, Value}
    end;
get(Json, Index) when is_list(Json) and is_integer(Index) ->
    case Index =< length(Json) of
	false -> {error, not_found};
	true -> {ok, lists:nth(Index, Json)}
    end;
get(Json, Path) when is_list(Path) ->
    get_list(Json, Path);
get(_Json, _Path) ->
    % !!! not too happy about obscuring badarg
    {error, not_found}.

-spec get_list(json(), list(path())) -> json().
get_list(Json, []) ->
    Json;
get_list(Json, [Tuple_elem | Tuple]) ->
    case get(Json, Tuple_elem) of
	{error, not_found} -> {error, not_found};
	{ok, Json2} -> get_list(Json2, Tuple)
    end.

-spec set(json(), path(), json()) -> json().
set(Json, Path, Value) ->
    update(Json, Path, fun (_T) -> Value end, Value).

-spec update(json(), path(), fun((json()) -> json()), json()) -> json().
update(Json, Path, F, Default) when is_atom(Path) ->
    update(Json, list_to_binary(atom_to_list(Path)), F, Default);
update({struct, Json}, Path, F, Default) when is_list(Json) and is_binary(Path) ->
    case lists:keyfind(Path, 1, Json) of
	{Path, Value} -> {struct, lists:keyreplace(Path, 1, Json, {Path, F(Value)})};
	false -> {struct, [{Path, Default} | Json]}
    end;
update(Json, Index, F, _Default) when is_list(Json) and is_integer(Index) ->
    % !!! behaviour for out of range indexes?
    set_nth(Index, Json, F(lists:nth(Index, Json)));
update(Json, Path, F, Default) when is_list(Path) ->
    update_list(Json, Path, F, Default).

-spec update_list(json(), list(path()), fun((json()) -> json()), json()) -> json().
update_list(Json, [], F, _Default) ->
    F(Json);
update_list(Json, [Tuple_elem | Tuple], F, Default) ->
    update(Json, Tuple_elem, fun (T) -> update_list(T, Tuple, F, Default) end, Default).

-spec set_nth(integer(), list(), term()) -> list().
set_nth(1, [_Head | Tail], Value) ->
    [Value | Tail];
set_nth(N, [Head | Tail], Value) when N>1 ->
    [Head | set_nth(N-1, Tail, Value)].

% decoding json body

-type decoder_arg() :: atom() | {atom(), fun((json:json()) -> term())}.

-spec decode_request_body(wrq:rd(), list(decoder_arg())) -> {ok, list(term())} | {error, term()}.
decode_request_body(ReqData, Args) ->
    try mochijson2:decode(wrq:req_body(ReqData)) of
	Body ->
	    Values = 
		lists:map(
		  fun ({Key, Decoder}) ->
			  {ok, Value} = json:get(Key, Body),
			  Decoder(Value);
		      (Key) ->
			  {ok, Value} = json:get(Key, Body),
			  Value
		  end,
		  Args
		 ),
	    {ok, Values}
    catch
	_:Error ->
	    {error, Error}
    end.
