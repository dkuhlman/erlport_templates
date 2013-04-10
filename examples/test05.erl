-module(test05).
-export([test01/2, test01/3]).

test01(NumProcesses, NumRequests) ->
    FileName = "client01.xml",
    test01(NumProcesses, NumRequests, FileName).

test01(NumProcesses, NumRequests, FileName) ->
    Request = {search, FileName, ".//name/text()"},  % text in all <name> tags
    Requests = make_request_list(NumRequests, Request, []),
%~     io:format("Requests: ~p~n", [Requests]),
    requestor05test:run(NumProcesses, Requests).

make_request_list(0, _, Requests) -> Requests;
make_request_list(NumRequests, Request, Requests) ->
    make_request_list(NumRequests - 1, Request, [Request|Requests]).
