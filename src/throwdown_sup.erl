%% @doc This is the main supervisor. It starts two child supervisors: one for
%% arenas and one for player instances.

-module(throwdown_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
         start_link/0,
         init/1
        ]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

sup_spec(Name) ->
    #{id       => Name,
      start    => {Name, start_link, []},
      restart  => permanent,
      shutdown => 2000,
      type     => supervisor,
      modules  => [Name]}.

init([]) ->
    SupFlags = #{ strategy => one_for_one },
    Children = [ sup_spec(throwdown_arena_sup), sup_spec(throwdown_player_sup) ],
    {ok, {SupFlags, Children}}.
