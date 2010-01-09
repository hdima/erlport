-module(hello).
-export([hello/1]).

hello(Name) when is_list(Name) ->
    Port = open_port({spawn, "python -u hello.py"},
        [{packet, 1}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({hello, Name})),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            binary_to_term(Data)
    after
        500 ->
            {error, timeout}
    end.
