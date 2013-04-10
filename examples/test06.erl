-module(test06).
-export([test01/2, test01/3, test01/4]).

test01(NumProcesses, NumRequests) ->
    FileName = "client01.xml",
    test01(NumProcesses, NumRequests, FileName).

test01(NumProcesses, NumRequests, FileName) ->
    Delay = 5000,
    test01(NumProcesses, NumRequests, FileName, Delay).

test01(NumProcesses, NumRequests, FileName, Delay) ->
    PythonModule = "client06test",
    Request = {search, FileName, ".//name/text()"},  % text in all <name> tags
    Requests = make_request_list(NumRequests, Request, []),
%~     io:format("Requests: ~p~n", [Requests]),
    requestor06testa:run(
        NumProcesses,
        Requests,
        PythonModule,
        requestor06testa,
        Delay).

make_request_list(0, _, Requests) -> Requests;
make_request_list(NumRequests, Request, Requests) ->
    make_request_list(NumRequests - 1, Request, [Request|Requests]).
