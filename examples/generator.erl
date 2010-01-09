-module(generator).
-export([generate/1]).

generate(N) when is_integer(N) andalso N >= 0 ->
    Port = open_port({spawn, "python -u handler.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    generate(Port, N),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            binary_to_term(Data)
    after
        500 ->
            {error, timeout}
    end.

generate(Port, 0) ->
    port_command(Port, term_to_binary(stop));
generate(Port, N) ->
    port_command(Port, term_to_binary(now())),
    generate(Port, N - 1).
