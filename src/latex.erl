-module(latex).

-export([ast/1, hash/1]).

-spec interact(string(), binary()) -> Reply :: binary().
interact(Command, Request) when is_list(Command) and is_binary(Request) ->
    Port = open_port({spawn, Command},[{packet, 2}, binary]),
    try
	port_command(Port, Request),
	receive
	    {Port, {data, Data}} -> Data
	after 
	    1000 -> erlang:error('latex.timeout')
	end
    after
	port_close(Port)
    end.

-spec ast(binary()) -> Ast :: binary().
ast(Latex) ->
    interact("./src/latex_ast", Latex).

-spec hash(binary()) -> Hash :: binary().
hash(Latex) ->
    interact("./src/latex_hash", Latex).
    
