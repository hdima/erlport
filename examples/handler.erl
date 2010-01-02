-module(handler).
-export([handle/1]).

handle(N) when N >= 0 ->
    Port = open_port({spawn, "python generator.py"},
        [{packet, 1}, nouse_stdio, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary(N)),
    handle(Port, []).

handle(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            case binary_to_term(Data) of
                stop ->
                    port_close(Port),
                    lists:reverse(Acc);
                Term ->
                    handle(Port, [Term | Acc])
            end
    end.
