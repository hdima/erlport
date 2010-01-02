-module(generator).
-export([generate/1]).

generate(N) when N >= 0 ->
    Port = open_port({spawn, "python handler.py"},
        [{packet, 1}, nouse_stdio, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    generate(Port, N),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            binary_to_term(Data)
    end.

generate(Port, 0) ->
    port_command(Port, term_to_binary(stop));
generate(Port, N) ->
    port_command(Port, term_to_binary(now())),
    generate(Port, N - 1).
