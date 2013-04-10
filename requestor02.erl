-module(requestor02).
-export([start/0, quit/1,
        fix_request1/2, fix_request2/2, fix_request3/2]).


%% External API

start() ->
    Port = open_port({spawn, "python -u client02.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    Port.

quit(Port) ->
    port_close(Port),
    ok.

fix_request1(Port, Filename) ->
    port_command(Port, term_to_binary({fix_request1, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

fix_request2(Port, Filename) ->
    port_command(Port, term_to_binary({fix_request2, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

fix_request3(Port, Filename) ->
    port_command(Port, term_to_binary({fix_request3, Filename},
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
