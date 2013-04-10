-module(requestor04).
-export([start/0, quit/1, rpc/2]).


%% External API

start() ->
    Port = open_port({spawn, "python -u client04.py"},
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
                {name_address, Data2} ->
                    io:format("Names and addresses~n----------------------~n"),
                    show_data(Data2, 0);
                {name_interest, Data2} ->
                    io:format("Interests~n----------------------~n"),
                    show_name_interest(Data2, 0)
            end
    after
        1000 ->
            {error, timeout}
    end.


%% Internal functions

show_data([], _) -> ok;
show_data([{Name, Address}|Rest], N) ->
    N1 = N + 1,
    io:format("~B. Name: ~s  Address: ~s~n", [N1, Name, Address]),
    show_data(Rest, N1).

show_name_interest([], _) -> ok;
show_name_interest([{Name, Interests}|Rest], N) ->
    N1 = N + 1,
    io:format("~B. Name: ~s:~n", [N1, Name]),
    show_interests(Interests),
    show_name_interest(Rest, N1).

show_interests([]) -> ok;
show_interests([Interest|Rest]) ->
    io:format("    ~s~n", [Interest]),
    show_interests(Rest).
