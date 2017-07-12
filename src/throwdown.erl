%% @doc This is the main convenience API for the game.

-module(throwdown).

-export([
         start/0,
         get_env/1,
         get_env/2,
         start_arena/1,
         start_player/2,
         start_game/1,
         start_game/2
        ]).

start() ->
    application:ensure_all_started(throwdown).

get_env(Key) ->
    get_env(Key, undefined).

get_env(Key, Default) ->
    case application:get_env(throwdown, Key) of
        {ok, V} -> V;
        undefined -> Default
    end.

%% @doc Start an arena in the default way and return its pid.
start_arena(Name) ->
    throwdown_arena_sup:start_arena(Name).

%% @doc Start a player with the given name and register it in the given arena.
start_player(Name, Arena) ->
    throwdown_player_sup:start_player(Name, Arena).

%% @doc Convenience function to start a game with a list of two or more player
%% names. The arena is given the default name of `<<"arena">>'. Returns a map
%% with game data. The map has the following keys: `arena' and `players'.
start_game(Players) when length(Players) > 1 ->
    start_game(<<"arena">>, Players).

%% @doc Convenience function to start a game with a given arena name and a list
%% of two or more players. Returns a map with game data using the following
%% keys: `arena' and `players'.
start_game(ArenaName, Players) when length(Players) > 1 ->
    {ok, ArenaPid} = start_arena(ArenaName),
    PMap = maps:from_list(
                [ begin
                      {ok, P} = start_player(N, ArenaPid),
                      {N, P}
                  end  || N <- Players ]
               ),

    throwdown_arena:done(ArenaPid),

    #{ arena => ArenaPid, players => PMap }.

