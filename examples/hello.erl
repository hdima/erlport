-module(hello).
-export([hello/1]).

hello(Name) ->
    Port = open_port({spawn, "python hello.py"},
        [{packet, 1}, nouse_stdio, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({hello, Name})),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            binary_to_term(Data)
    end.
