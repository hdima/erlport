-module(compression).
-export([echo_big_data/1]).

echo_big_data(N) when N > 0 ->
    Port = open_port({spawn, "python -u compression.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    Big = lists:seq(0, N),
    port_command(Port, term_to_binary({echo, Big}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            Big = binary_to_term(Data)
    after
        500 ->
            {error, timeout}
    end.
