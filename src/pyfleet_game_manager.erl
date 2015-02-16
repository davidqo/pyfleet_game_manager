%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 12:54
%%%-------------------------------------------------------------------
-module(pyfleet_game_manager).
-author("davidqo").

-behaviour(gen_server).

-include("../include/pyfleet_game_manager.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-define(MAX_GAME_COUNT, 100).

-record(state, {
    sup_ref,
    game_sup_ref,
    running_game_count = 0,
    max_game_count = ?MAX_GAME_COUNT,
    running_game_list,
    game_id_queue :: queue:queue()
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link([SupRef :: pid()]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SupRef) ->
    io:format("Pyfleet game manager. Start link. Sup ref: ~p~n", [SupRef]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SupRef], []).

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
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([SupRef]) ->
  io:format("Pyfleet game manager init~n", []),
  self() ! init,
  IdQueue = queue:from_list(lists:seq(1, ?MAX_GAME_COUNT * 2)),
  {ok, #state{sup_ref = SupRef, game_id_queue = IdQueue}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call(#pyfleet_create_game{game_type = GameType, options = Options, player_name = PlayerName, player_ref = PlayerRef}, {_, _}, State = #state{game_sup_ref = GameSupRef, running_game_count = RunningGameCount, max_game_count = MaxGameCount, running_game_list = RunningGameList, game_id_queue = GameIdQueue}) when RunningGameCount < MaxGameCount ->
  io:format("Pyfleet game manager. Create game~n", []),
  case pyfleet_game:create_game(GameType, GameSupRef, PlayerName, PlayerRef, Options) of
    {ok, GameRef} ->
      io:format("Game ~p is created~n", [GameRef]),
      {GameId, GameIdQueue2} = get_game_id(GameIdQueue),
      {reply, {ok, GameRef}, State#state{running_game_count = RunningGameCount + 1, game_id_queue = GameIdQueue2, running_game_list = [#game_description{id = GameId, player_count = 1, game_ref = GameRef, player_list = [{PlayerName, PlayerRef}], options = Options} | RunningGameList]}};
    {error, Cause} ->
      {reply, {error, Cause}, State}
  end;
handle_call(#pyfleet_join_game{game_ref = GameRef, player_name = PlayerName, player_ref = PlayerRef}, From, State = #state{running_game_list = RunningGameList}) ->
  io:format("Pyfleet game manager. Player ~p join game~n", [PlayerName]),
  case lists:keytake(GameRef, #game_description.game_ref, RunningGameList) of
    {value, GameDescription = #game_description{player_list = PlayerList}, GameList2} ->
      GameDescription2 = GameDescription#game_description{player_list = lists:keystore(PlayerName, 1, PlayerList, {PlayerName, PlayerRef})},
      pyfleet_game:join_game(GameRef, From, PlayerName),
      {noreply, State#state{running_game_list = [GameDescription2 | GameList2]}};
    false ->
      {reply, {error, no_such_game}, State}
  end;
handle_call(Request, _From, State) ->
  io:format("Unexpected call: ~p~n", [Request]),
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).


handle_cast(Msg, State) ->
  io:format("Pyfleet player manager. Unexpected cast: ~p~n", [Msg]),
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_info({'DOWN', MonitorRef, _, _, _}, State = #state{running_game_count = RunningGameCount, running_game_list = RunningGameList, game_id_queue = GameIdQueue}) ->
  io:format("Game manager. Game stopped..~n", []),
  case lists:keytake(MonitorRef, #game_description.game_ref, RunningGameList) of
    {value, #game_description{id = GameId}, RestGameList} ->
      io:format("Game manager. Game ~p is down~n", [GameId]),
      {noreply, State#state{running_game_count = RunningGameCount - 1, running_game_list = RestGameList, game_id_queue = free_game_id(GameId, GameIdQueue)}};
    false ->
      io:format("Game manager. Error: unknown game stopped~n", []),
      {noreply, State}
    end;
handle_info(init, State = #state{sup_ref = SupRef}) ->
  io:format("Game manager. Initialization..~n", []),
  {ok, UserSupRef} = create_game_worker_supervisor(SupRef),
  io:format("Game manager. Initialization.. done!~n", []),
  {noreply, State#state{game_sup_ref = UserSupRef}};
handle_info(Info, State) ->
  io:format("Game manager. Unexpected info: ~p~n", [Info]),
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_game_worker_supervisor(SupRef) ->
  ChildSpec =
    {pyfleet_game_worker_supervisor,
    {pyfleet_game_worker_sup, start_link, []},
    temporary,
    10000,
    supervisor,
    [pyfleet_game_worker_sup]},
  supervisor:start_child(SupRef, ChildSpec).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++

get_game_id(IdQueue) ->
  case queue:out(IdQueue) of
    {{value, Id}, Queue2} ->
      {Id, Queue2};
    _ ->
      throw(cannot_get_id)
  end.

free_game_id(Id, IdQueue) ->
  queue:in_r(Id, IdQueue).