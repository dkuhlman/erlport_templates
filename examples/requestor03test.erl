-module(requestor03test).
-export([start/0, stop/1, rpc/2, runclient/2, runinterest/2, rundescription/2]).


%% External API

start() ->
    Port = open_port({spawn, "python -u client03.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    Port.

stop(Port) ->
    port_close(Port),
    ok.

% Send a request.  Request is of the form:
%     {names, Filename}
%     {interestnames, Filename}
%     {interestdescriptions, Filename}
rpc(Port, Request) ->
    port_command(Port, term_to_binary(Request, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            case Data1 of
                {names, Data2} ->
                    io:format("Names~n----------------------~n"),
                    show_strings(Data2, 0);
                {interestnames, Data2} ->
                    io:format("Interests~n----------------------~n"),
                    show_strings(Data2, 0);
                {descriptions, Size, Data2} ->
                    io:format("Descriptions~n----------------------~n"),
                    show_strings(Data2, 0),
                    io:format("Size: ~B~n", [Size])
            end
    after
        500 ->
            {error, timeout}
    end.


runclient(Port, Filename) ->
    port_command(Port, term_to_binary({names, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
%~             Length = lists:flatlength(Data1),
%~             io:format("Data: ~p~nCount: ~B~n", [Data1, Length])
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

runinterest(Port, Filename) ->
    port_command(Port, term_to_binary({interestnames, Filename}, [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
%~             Length = lists:flatlength(Data1),
%~             io:format("Data: ~p~nCount: ~B~n", [Data1, Length])
            show_strings(Data1, 0)
    after
        500 ->
            {error, timeout}
    end.

rundescription(Port, Filename) ->
    port_command(Port, term_to_binary({interestdescriptions, Filename},
            [compressed])),
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
%~             Length = lists:flatlength(Data1),
%~             io:format("Data: ~p~nCount: ~B~n", [Data1, Length])
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
