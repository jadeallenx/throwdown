-module(throwdown_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
         start_link/0,
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

player_spec() ->
    #{id       => undefined,
      start    => {throwdown_player, start_link, []},
      restart  => temporary,
      shutdown => 2000,
      type     => worker,
      modules  => [throwdown_player]}.

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one },
    Children = [ throwdown_arena:child_spec(), player_spec() ],
    {ok, {SupFlags, Children}}.
