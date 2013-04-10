-module(requestor04test).
-export([run/2]).

%%
%% Create Count number of erlport/python processes and process a list
%%   of requests on them.
%% A list of requests might look like the following:
%%     Requests = [
%%         {search, File_name, Xpath_search_string},
%%         {search, File_name, Xpath_search_string},
%%         ].
%% For example:
%%       Requests = [
%%           {search, "test01.xml", ".//name/text()"}  % text in all <name> tags
%%           ]
%%

%% External API

run(Tasks, NumProcesses) ->
    Processes = make_processes(NumProcesses, []),
    {ok, Tasks1, N} = start_processes(Tasks, Processes, 0),
    loop(Tasks1, N).

%% Internal functions

loop(Tasks, N) ->
    receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            case Data1 of
                {fix_request1, N2, Data2} ->
                    %
                    % Add your processing here.
                    %
                    FileName = io_lib:format("./Results/out~B.txt", [N2]),
                    {ok, IoDevice} = file:open(FileName, write),
                    io:format(IoDevice, "~n----------------------~n", []),
                    io:format(IoDevice, "N2: ~B  Data2: ~p~n",
                        [N2, Data2]),
                    file:close(IoDevice);
                {fix_request2, N2, Data2} ->
                    %
                    % Add your processing here.
                    %
                    FileName = io_lib:format("./Results/out~B.txt", [N2]),
                    {ok, IoDevice} = file:open(FileName, write),
                    io:format(IoDevice, "~n----------------------~n", []),
                    io:format(IoDevice, "N2: ~B  Data2: ~p~n",
                        [N2, Data2]),
                    file:close(IoDevice);
                {fix_request3, N2, Data2} ->
                    %
                    % Add your processing here.
                    %
                    FileName = io_lib:format("./Results/out~B.txt", [N2]),
                    {ok, IoDevice} = file:open(FileName, write),
                    io:format(IoDevice, "~n----------------------~n", []),
                    io:format(IoDevice, "N2: ~B  Data2: ~p~n",
                        [N2, Data2]),
                    file:close(IoDevice)
            end,
            case Tasks of
                [] ->
                    io:format("Closing port: ~p~n", [Port]),
                    port_close(Port),
                    loop([], N + 1);
                [Request|Tasks1] ->
                    N1 = N + 1,
                    start_1_process(Request, Port, N1),
                    loop(Tasks1, N1)
            end
    after
        1000 ->
            {error, timeout}
    end.

start_processes([], _, N) -> {ok, [], N};
start_processes(Tasks, [], N) -> {ok, Tasks, N};
start_processes([Request|Tasks], [Port|Processes], N) ->
    start_1_process(Request, Port, N),
    start_processes(Tasks, Processes, N + 1).

start_1_process(Request, Port, N) ->
    {Key, FileName, SearchStr} = Request,
    Request1 = {Key, N, FileName, SearchStr},
    io:format("Starting -- Request1: ~p~n", [Request1]),
    port_command(Port, term_to_binary(Request1, [compressed])).

make_processes(0, Processes) -> Processes;
make_processes(N, Processes) ->
    Port = open_port({spawn, "python -u client04test.py"},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    make_processes(N - 1, [Port|Processes]).

%~ show_data([], _) -> ok;
%~ show_data([Str|Rest], N) ->
%~     N1 = N + 1,
%~     io:format("~B. ~s~n", [N1, Str]),
%~     show_data(Rest, N1).
