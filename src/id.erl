-module(id).

-include("types.hrl").

-export([new/1, type/1]).

-define(LEN, 32).

-spec new(id_type()) -> id().
new(Type) ->
    Prefix = 
	case Type of
	    formula -> <<"form">>;
	    challenge -> <<"chal">>;
	    response -> <<"resp">>
	end,
    Random = crypto:rand_bytes(?LEN - 4),
    Suffix = iolist_to_binary([io_lib:format("~2.36.0b", [Byte]) || Byte <- binary_to_list(Random)]),
    << Prefix/binary, Suffix/binary >>. 
			   
-spec type(id()) -> id_type().
type(<<"form", _/binary>>) ->
    formula;
type(<<"chal", _/binary>>) ->
    challenge;
type(<<"resp", _/binary>>) ->
    response.
