%% @doc This is the supervisor which starts processes to hold player state.
%% Player processes are responsible for making using a strategy and using that
%% strategy to make a choice in an arena.

-module(throwdown_player_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
         start_link/0,
         start_player/2,
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_player(Name, Arena) ->
    supervisor:start_child(?SERVER, [Name, Arena]).

player_spec() ->
    #{id       => undefined,
      start    => {throwdown_player, start_link, []},
      restart  => temporary,
      shutdown => 2000,
      type     => worker,
      modules  => [throwdown_player]}.

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one },
    Children = [ player_spec() ],
    {ok, {SupFlags, Children}}.
