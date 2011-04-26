-module(latex).

-export([ast/1, hash/1]).

-spec interact(string(), binary()) -> {'ok', Reply :: binary()} | timeout.
interact(Command, Request) when is_list(Command) and is_binary(Request) ->
    Port = open_port({spawn, Command},[{packet, 2}, binary]),
    try
	port_command(Port, Request),
	receive
	    {Port, {data, Data}} -> {ok, Data}
	after 
	    1000 -> timeout
	end
    after
	port_close(Port)
    end.

-spec ast(binary()) -> {'ok', Ast :: binary()} | timeout.
ast(Latex) ->
    interact("./src/latex_ast", Latex).

-spec hash(binary()) -> {'ok', Hash :: binary()} | timeout.
hash(Latex) ->
    interact("./src/latex_hash", Latex).
    
