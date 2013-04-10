-module(requestor01).
-export([fix_request1/1, fix_request2/1, fix_request3/1]).


%% External API

fix_request1(Arg1) ->
    Port = open_port({spawn, "python -u client01.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({fix_request1, Arg1}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            Data1 = binary_to_term(Data),
            show_data(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

fix_request2(Arg1) ->
    Port = open_port({spawn, "python -u client01.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({fix_request2, Arg1}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            Data1 = binary_to_term(Data),
            show_data(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

fix_request3(Arg1) ->
    Port = open_port({spawn, "python -u client01.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    port_command(Port, term_to_binary({fix_request3, Arg1}, [compressed])),
    receive
        {Port, {data, Data}} ->
            port_close(Port),
            Data1 = binary_to_term(Data),
            {Size, Data2} = Data1,
            show_data(Data2, 0),
            io:format("Size: ~B~n", [Size])
    after
        500 ->
            {error, timeout}
    end.


%% Internal functions

show_data([], _) -> ok;
show_data([Str|Rest], N) ->
    N1 = N + 1,
    io:format("~B. ~s~n", [N1, Str]),
    show_data(Rest, N1).
