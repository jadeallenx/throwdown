-module(throwdown).

-export([
         start/0,
         get_env/1,
         get_env/2,
         start_arena/1,
         start_player/2,
         start_game/1
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

start_arena(Name) ->
    throwdown_arena_sup:start_arena(Name).

start_player(Name, Arena) ->
    throwdown_player_sup:start_player(Name, Arena).

start_game(Players) when length(Players) > 1 ->
    {ok, ArenaPid} = start_arena(<<"arena">>),
    Players = maps:from_list(
                [ begin
                      {ok, P} = start_player(N, ArenaPid),
                      {N, P}
                  end  || N <- Players ]
               ),

    throwdown_arena:done(ArenaPid),

    #{ arena => ArenaPid, players => Players }.

