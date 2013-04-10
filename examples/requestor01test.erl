-module(requestor01test).
-export([runclient/1, runinterest/1, rundescription/1]).


%% External API

runclient(Filename) ->
    Port = open_port({spawn, "python -u client01.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({names, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            Data1 = binary_to_term(Data),
%~             Length = lists:flatlength(Data1),
%~             io:format("Data: ~p~nCount: ~B~n", [Data1, Length])
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

runinterest(Filename) ->
    Port = open_port({spawn, "python -u client01.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({interestnames, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            Data1 = binary_to_term(Data),
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

rundescription(Filename) ->
    Port = open_port({spawn, "python -u client01.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({interestdescriptions, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
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
