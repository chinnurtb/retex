-module(latex).

-export([ast/1, hash/1]).

interact(Command, Request) ->
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

ast(Latex) ->
    interact("./src/latex_ast", Latex).

hash(Latex) ->
    interact("./src/latex_hash", Latex).
    
