-module(latex).

-export([ast/2, hash/2]).

interact(Command, Request, Timeout) ->
    Port = open_port({spawn, Command},[{packet, 2}, binary]),
    try
	port_command(Port, Request),
	receive
	    {Port, {data, Data}} -> {ok, Data}
	after 
	    Timeout -> timeout
	end
    after
	port_close(Port)
    end.

ast(Latex, Timeout) ->
    interact("./latex_ast", Latex, Timeout).

hash(Latex, Timeout) ->
    interact("./latex_hash", Latex, Timeout).


