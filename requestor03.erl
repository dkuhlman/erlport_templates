-module(requestor03).
-export([start/0, quit/1, rpc/2]).


%% External API

start() ->
    Port = open_port({spawn, "python -u client03.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    Port.

quit(Port) ->
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
                {fix_request1, Data2} ->
                    %
                    % Add your processing here.
                    %
                    io:format("Names~n----------------------~n"),
                    show_data(Data2, 0);
                {fix_request2, Data2} ->
                    %
                    % Add your processing here.
                    %
                    io:format("Interests~n----------------------~n"),
                    show_data(Data2, 0);
                {fix_request3, Size, Data2} ->
                    %
                    % Add your processing here.
                    %
                    io:format("Descriptions~n----------------------~n"),
                    show_data(Data2, 0),
                    io:format("Size: ~B~n", [Size])
            end
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
