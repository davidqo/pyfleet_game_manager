%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. фев 2015 14:54
%%%-------------------------------------------------------------------
-module(pyfleet_game_manager_sup).
-author("davidqo").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link(?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {one_for_all, 1000, 3600},
    Child = {'pyfleet_game_manager', {'pyfleet_game_manager', start_link, [self()]},
        transient, 2000, worker, ['pyfleet_game_manager']},
    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
