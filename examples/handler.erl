-module(handler).
-export([handle/1]).

handle(N) when is_integer(N) andalso N >= 0 ->
    Port = open_port({spawn, "python -u generator.py"},
        [{packet, 1}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
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
    after
        5000 ->
            {error, timeout}
    end.
