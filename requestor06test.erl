-module(requestor06test).
-behavior(requestor06behavior).
-export([run/5]).
-export([process_result/4]).

%
% The run() function in the behavior module, in this case, does what
% we want it to, so we just call it.
run(NumProcesses, Tasks, PythonModule, Mod, Delay) ->
    requestor06behavior:run(NumProcesses, Tasks, PythonModule, Mod, Delay).

%
% This implements the way we want to process the data returned by
% the Python process.
process_result(search, ReturnNProcess, ReturnNTask, ReturnData) ->
    io:format("NProcess: ~B  NTask: ~B  ReturnData: ~p~n", [
            ReturnNProcess, ReturnNTask, ReturnData]),
    io:format("    ReturnNProcess: ~B  ReturnNTask: ~B~n", [
            ReturnNProcess, ReturnNTask]),
    ok.
