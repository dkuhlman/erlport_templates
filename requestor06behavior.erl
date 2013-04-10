-module(requestor06behavior).
-export([run/5]).
% Callbacks
%~ -export([process_result/5]).

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
%% This template creates a fixed number of erlang processes.  Each erlang
%%   process is paired with a python process (identified by a port).

%
% Callback declarations

-callback process_result(
        Key :: atom(),
        ReturnNProcess :: integer(),
        ReturnNTask :: integer(),
        ReturnData :: term()) ->
    ok |
    {error, atom()}.

%% External API

%
% The user's erlang module should call this function to start processing.
run(NumProcesses, Tasks, PythonModule, Mod, Delay) ->
    Processes = make_processes(NumProcesses, [], PythonModule, Mod, Delay),
    ok = start_processes(Processes),
    main_request_loop(Tasks, 0, NumProcesses),
    ok.

%
% Callbacks -- must be implemented in client module.

%
% Each time the python client process returns result data, process_result
% is called.  The user's erlang module should implement this function
% to do something with that result data.
process_result(Key, ReturnNProcess, ReturnNTask, ReturnData, Mod) ->
    Mod:process_result(Key, ReturnNProcess, ReturnNTask, ReturnData),
    ok.

%% Internal functions

%
% Respond to requests/messages from the individual worker (erlang) processes.
% In response to a get_task message, send back the next task.
% When there are no more tasks, send back an end_of_tasks message so that
% the process can clean-up and exit.  When there are no more processes, exit.
main_request_loop(Tasks, NTask, NumProcesses) ->
    receive
        {From, get_task} ->
            case Tasks of
                [Task|Rest] ->
                    NTask1 = NTask + 1,
                    From ! {next_task, Task, NTask1},
                    main_request_loop(Rest, NTask1, NumProcesses);
                [] ->
                    From ! end_of_tasks,
                    case NumProcesses of
                        1 ->
                            % Exit the main loop when we have sent the
                            % end_of_tasks message to all worker processes.
                            ok;
                        _ ->
                            main_request_loop([], -1, NumProcesses - 1)
                    end
            end
    end.

%
% Create and return N erlang processes.
% Each process has an associated python process.
make_processes(0, Processes, _, _, _) -> Processes;
make_processes(N, Processes, PythonModule, Mod, Delay) ->
    SupervisorPid = self(),
    F1 = fun() -> erl_process(SupervisorPid, N, PythonModule, Mod, Delay) end,
    Pid = spawn(F1),
    make_processes(N - 1, [Pid|Processes], PythonModule, Mod, Delay).

%
% This is the erlang worker process.
% It (1) creates the python process (i.e. opens the port); (2) starts
% the worker process loop; and (3) when the loop exits, closes the port.
erl_process(SupervisorPid, NProcess, PythonModule, Mod, Delay) ->
    Cmd = lists:flatten(io_lib:format("python -u ~s.py", [PythonModule])),
    Port = open_port({spawn, Cmd},
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    worker_process_loop(SupervisorPid, Port, NProcess, Mod, Delay),
    % When the worker_process_loop exits, we close the port, because
    % we no longer need it.
    port_close(Port),
    ok.

%
% Start each of the processes.  In other words, send each process a
% start_task message.
start_processes([]) -> ok;
start_processes([Process|Processes]) ->
    Process ! start_task,
    start_processes(Processes).

%
% Send one command/request to a python process/port.
send_command_to_python(Request, Port, NProcess, NTask) ->
    {Key, FileName, SearchStr} = Request,
    Request1 = {Key, NProcess, NTask, FileName, SearchStr},
    %io:format("Starting -- Request1: ~p~n", [Request1]),
    io:format("NProcess:       ~B  NTask:       ~B~n", [NProcess, NTask]),
    port_command(Port, term_to_binary(Request1, [compressed])).

%
% (1) Start the first task.
% (2) Each time we get results back from the python process associated
%     with this worker process, do something with those results and then
%     get the next task and send that request/task to the python process.
% (3) Exit when there are no more tasks (i.e. we get an end_of_tasks
%     message back from the supervisor process).
worker_process_loop(SupervisorPid, Port, NProcess, Mod, Delay) ->
    receive
        start_task ->
            % Get a task and start it in the Python process.
            SupervisorPid ! {self(), get_task},
            receive
                {next_task, Task, NTask} ->
                    send_command_to_python(Task, Port, NProcess, NTask),
                    worker_process_loop(
                        SupervisorPid,
                        Port,
                        NProcess,
                        Mod,
                        Delay);
                end_of_tasks ->
                    % No more tasks.  Close port and quit.
                    ok
            end;
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            %io:format("Data1: ~p~n", [Data1]),
            case Data1 of
                {task_response, RequestCode, ReturnNProcess,
                    ReturnNTask, ReturnData} ->
                    %
                    % We've received results/response from the Python process.
                    % Do your result processing here.
                    %
                    process_result(
                        RequestCode,
                        ReturnNProcess,
                        ReturnNTask,
                        ReturnData,
                        Mod),
                    % Get another task and start it in the Python process.
                    SupervisorPid ! {self(), get_task},
                    receive
                        {next_task, Task, NTask} ->
                            send_command_to_python(Task, Port, NProcess, NTask),
                            worker_process_loop(
                                SupervisorPid,
                                Port, NProcess,
                                Mod,
                                Delay);
                        end_of_tasks ->
                            % No more tasks.  Close port and quit.
                            ok
                    end
            end
    after Delay ->
            io:format("No response from port.  Port: ~p~n", [Port]),
            ok
    end.
