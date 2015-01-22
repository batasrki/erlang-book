%%%-------------------------------------------------------------------
%%% File    : tr_server.erl
%%% Author  : Srdjan Pejic <spejic@freya.local>
%%% Description : RPC over TCP
%%%
%%% Created : 21 Jan 2015 by Srdjan Pejic <spejic@freya.local>
%%%-------------------------------------------------------------------
-module(tr_server).

-behaviour(gen_server).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1, get_count/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(DEFAULT_PORT, 1055).
-define(SERVER, ?MODULE).

-record(state, {port, listen_socket, request_count = 0}).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server on a given port
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).


%%--------------------------------------------------------------------
%% Function: start_link/0
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    start_link(?DEFAULT_PORT).

%%--------------------------------------------------------------------
%% Function: get_count/0
%% Description: Fetches the number of requests made so far
%%--------------------------------------------------------------------
get_count() ->
    gen_server:call(?SERVER, get_count).

%%--------------------------------------------------------------------
%% Function: stop/0
%% Description: Stops the server
%%--------------------------------------------------------------------
stop() ->
    gen_server:cast(?SERVER, stop).

%%====================================================================
%% Server functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active, true}]),
    {ok, #state{port = Port, listen_socket = ListenSocket}, 0}.

%%--------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(get_count, _From, State) ->
    {reply, {ok, State#state.request_count}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({tcp, Socket, RawData}, State) ->
    do_rpc(Socket, RawData),
    RequestCount = State#state.request_count,
    {noreply, State#state{request_count = RequestCount + 1}};
handle_info(timeout, #state{listen_socket = ListenSocket} = State) ->
    {ok, _Sock} = gen_tcp:accept(ListenSocket),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_rpc(Socket, RawData) ->
    try
        {Mod, Func, Args} = split_out_mfa(RawData),
        Result = apply(Mod, Func, Args),
        gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Result]))
    catch
        _Class:Err ->
            gen_tcp:send(Socket, io_lib:fwrite("~p~n", [Err]))
    end.

split_out_mfa(RawData) ->
    MFA = re:replace(RawData, "\r\n$", "", [{return, list}]),
    {match, [Mod, Func, Args]} =
        re:run(MFA, "(.*):(.*)\s*\\((.*)\s*\\)\s*.\s*$",
               [{capture, [1,2,3], list}, ungreedy]),
    {list_to_atom(Mod), list_to_atom(Func), args_to_terms(Args)}.

args_to_terms(RawArgs) ->
    {ok, Tokens, _Line} = erl_scan:string("[" ++ RawArgs ++ "]. ", 1),
    {ok, Args} = erl_parse:parse_term(Tokens),
    Args.

%%--------------------------------------------------------------------
%% Function: start_test/0
%% Description: Tests starting the server on a given port
%%--------------------------------------------------------------------
start_test() ->
    {ok, _} = tr_server:start_link(1055).
