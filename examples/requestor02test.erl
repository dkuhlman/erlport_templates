-module(requestor02test).
-export([start/0, stop/1,
        requestnames/2, requestinterest/2, requestdescription/2]).


%% External API

start() ->
    Port = open_port({spawn, "python -u client02.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    Port.

stop(Port) ->
    port_close(Port),
    ok.

requestnames(Port, Filename) ->
    port_command(Port, term_to_binary({names, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

requestinterest(Port, Filename) ->
    port_command(Port, term_to_binary({interestnames, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

requestdescription(Port, Filename) ->
    port_command(Port, term_to_binary({interestdescriptions, Filename},
            [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            {Size, Data2} = Data1,
            show_strings(Data2, 0),
            io:format("Size: ~B~n", [Size])
    after
        500 ->
            {error, timeout}
    end.


%% Internal functions

show_strings([], _) -> ok;
show_strings([Str|Rest], N) ->
    N1 = N + 1,
    io:format("~B. ~s~n", [N1, Str]),
    show_strings(Rest, N1).
