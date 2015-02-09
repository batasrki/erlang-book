%%%-------------------------------------------------------------------
%%% File    : tr_app.erl
%%% Author  : Srdjan Pejic <spejic@freya.local>
%%% Description : 
%%%
%%% Created :  8 Feb 2015 by Srdjan Pejic <spejic@freya.local>
%%%-------------------------------------------------------------------
-module(tr_app).

-behaviour(application).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
%%--------------------------------------------------------------------
-export([
         start/2,
         stop/1
        ]).

%%--------------------------------------------------------------------
%% Macros
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%% Intent: do start-up tasks in this function; things like initialising a database connection
%%--------------------------------------------------------------------

start(Type, StartArgs) ->
    case tr_sup:start_link() of
        {ok, Pid} -> 
            {ok, Pid};
        Other ->
            {error, Other}
    end.

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any 
%%--------------------------------------------------------------------
stop(State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
