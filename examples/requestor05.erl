%%%-------------------------------------------------------------------
%%% @author Dave Kuhlman <>
%%% @copyright (C) 2012, Dave Kuhlman
%%% @doc
%%%
%%% @end
%%% Created :  1 Dec 2012 by Dave Kuhlman <>
%%%-------------------------------------------------------------------
-module(requestor05).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([requestnames/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {available_processes, inuse_processes}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Specific requests
%%
%% @spec requestN() -> {ok, Result} | {error, timeout}
%% @end
%%--------------------------------------------------------------------

requestnames(Filename) ->
    gen_server:call(?MODULE, {requestnames, Filename}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([ProcessCount, Command]) ->
    Processes = make_processes(ProcessCount, Command, []),
    {ok, #state{available_processes=Processes, inuse_processes=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({requestnames, Filename}, _From, State) ->
    % must be able to wait here until a process is available.
    {Port, State1} = acquire_port(State),
    port_command(Port, term_to_binary({fix_request1, Filename}, [compressed])),
    Reply = receive
        {Port, {data, Data}} ->
            Data1 = binary_to_term(Data),
            show_strings(Data1, 0),
            ok
    after
        500 ->
            {error, timeout}
    end,
    {reply, Reply, State1}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{available_processes=Processes, inuse_processes=InUse}) ->
    kill_processes(Processes),
    case InUse of
        [] -> ok;
        _ -> processes_inuse
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

kill_processes([]) -> ok;
kill_processes([Process|Rest]) ->
    port_close(Process),
    kill_processes(Rest).

make_processes(0, _, Processes) ->
    Processes;
make_processes(N, Command, Processes) ->
    Port = open_port({spawn,Command},                              
        [{packet, 4}, binary, {env, [{"PYTHONPATH", "../src"}]}]),
    make_processes(N - 1, Command, [Port|Processes]).

show_strings([], _) -> ok;
show_strings([Str|Rest], N) ->
    N1 = N + 1,
    io:format("~B. ~s~n", [N1, Str]),
    show_strings(Rest, N1).

acquire_port(State) ->
    Port = 1,
    State1 = State,
    {Port, State1}.

