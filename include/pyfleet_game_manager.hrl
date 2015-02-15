%%%-------------------------------------------------------------------
%%% @author Davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Февр. 2015 17:52
%%%-------------------------------------------------------------------

-type game_type() :: pyfleet_translation | pyfleet.

-record(game_description, {
  id,
  game_ref,
  game_pid,
  player_count = 0,
  max_player_count = 4,
  player_list = [],
  options
}).

-record(pyfleet_create_game, {
  game_type :: game_type(),
  player_name,
  player_ref,
  options
}).

-record(pyfleet_join_game, {
  game_ref,
  player_name,
  player_ref
}).

-record(pyfleet_game_request, {
  player_ref,
  body
}).

-record(pyfleet_game_response, {
  player_ref,
  body
}).