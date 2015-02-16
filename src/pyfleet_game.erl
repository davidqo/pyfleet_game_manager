%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 14:05
%%%-------------------------------------------------------------------
-module(pyfleet_game).
-author("davidqo").

-behaviour(gen_server).

-include("../include/pyfleet_game_manager.hrl").

%% API
-export([create_game/5, join_game/3, start_link/3]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(battle_field, {
  dreadnoughts = [] :: [{PlayerName :: string(), Ships :: integer()}]
}).

-define(TICK_DURATION, 1000).
-define(MAX_TICK_COUNT, 2000).

-record(state, {
  status = wait_for_others :: wait_for_others | battle,
  player_list,
  max_players = 4,
  min_players = 2,
  player_count = 0,
  battle_field,
  ticks = 0,
  max_ticks = ?MAX_TICK_COUNT,
  options
}).

%%%===================================================================
%%% API
%%%===================================================================

create_game(pyfleet, SupRef, PlayerName, PlayerRef, Options) ->
  io:format("Pyfleet game. Create game. Game sup ref: ~p, Creator: ~p, Creator ref: ~p Options: ~p~n", [SupRef, PlayerName, PlayerRef, Options]),
  supervisor:start_child(SupRef, [PlayerName, PlayerRef, Options]).

join_game(GameRef, From, PlayerName) ->
  gen_server:call(GameRef, {join_game, From, PlayerName}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------

start_link(PlayerName, PlayerRef, Options) ->
  io:format("Pyfleet game. Start link. player name: ~p player ref: ~p, options: ~p", [PlayerName, PlayerRef, Options]),
  gen_server:start_link(?MODULE, [PlayerName, PlayerRef, Options], []).

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
init([PlayerName, From, Options]) ->
  io:format("Pyfleet game. Init~n", []),
  self() ! {create, From},
  PlayerRef =
    case From of
      {Pid, _} ->
        Pid;
      Pid ->
        Pid
    end,
  BattleField = #battle_field{dreadnoughts = [{PlayerName, 1000}]},
  {ok, #state{player_list = [{PlayerName, PlayerRef}], options = Options, player_count = 1, battle_field = BattleField}}.

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

handle_call({join_game, {Pid, _} = From, PlayerName}, _, State = #state{status = wait_for_others, player_list = PlayerList, min_players = MinPlayers, player_count = PlayerCount, battle_field = BattleField}) when (PlayerCount + 1) < MinPlayers ->
  io:format("Join game. Wait for others~n", []),
  gen_server:reply(From, joined),
  BattleField2 = add_player_fleet(PlayerName, BattleField),
  {reply, ok, State#state{status = battle, player_count = PlayerCount + 1, player_list = lists:keystore(PlayerName, 1, PlayerList, {PlayerName, Pid}), battle_field = BattleField2}};
handle_call({join_game, {Pid, _} = From, PlayerName}, _, State = #state{status = wait_for_others, player_list = PlayerList, player_count = PlayerCount, battle_field = BattleField}) ->
  io:format("Join game. Start battle~n", []),
  gen_server:reply(From, joined),
  io:format("Player ~p joined to game~n", [PlayerName]),
  timer:send_after(?TICK_DURATION, tick),
  BattleField2 = add_player_fleet(PlayerName, BattleField),
  {reply, ok, State#state{player_count = PlayerCount + 1, player_list = lists:keystore(PlayerName, 1, PlayerList, {PlayerName, Pid}), battle_field = BattleField2}};
handle_call(Request, _From, State) ->
  io:format("Pyfleet game. Unexpected request: ~p~n", [Request]),
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

handle_cast(_Request, State) ->
  io:format("Pyfleet game. Unknown cast~n", []),
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

handle_info(tick, State = #state{max_ticks = MaxTicks, ticks = Ticks, player_list = PlayerList}) when Ticks == MaxTicks ->
  io:format("Game time expired~n", []),
  send_to_subscribers(game_time_expired, PlayerList),
  {stop, {shutdown, game_time_expired}, State = #state{}};
handle_info(tick, State = #state{ticks = Ticks, battle_field = BattleField, player_list = PlayerList}) ->
  io:format("Tick: ~p~n", [Ticks]),
  BattleField2 = battle_tick(BattleField),
  timer:send_after(?TICK_DURATION, self(), tick),
  send_to_subscribers(BattleField2, PlayerList),
  {noreply, State#state{ticks = Ticks + 1, battle_field = BattleField2}};
handle_info({create, From}, State = #state{}) ->
  io:format("Notify Player game is created: ~p~n", [From]),
  case From of
    {_, _} ->
      gen_server:reply(From, created);
    Pid ->
      Pid ! created
  end,
  {noreply, State};
handle_info(Info, State) ->
  io:format("Unexpected info: ~p~n", [Info]),
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

battle_tick(BattleField = #battle_field{dreadnoughts = Dreadnoughts}) ->
  ReduceFun =
    fun
      ({User, Fleet}) ->
        {User, Fleet - 1}
    end,
  Dreadnoughts2 = lists:map(ReduceFun, Dreadnoughts),
  BattleField#battle_field{dreadnoughts = Dreadnoughts2}.
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

add_player_fleet(PlayerName, BattleField = #battle_field{dreadnoughts = Dreadnoughts}) ->
  BattleField#battle_field{dreadnoughts = [{PlayerName, 1000} | Dreadnoughts]}.

send_to_subscribers(_, []) ->
  ok;
send_to_subscribers(Msg, [{_, PlayerPid} | Tail]) ->
  PlayerPid ! Msg,
  send_to_subscribers(Msg, Tail).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++