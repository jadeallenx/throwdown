%% @doc This supervisor manages the arenas that manage game state, including
%% the  players who are currently participating in a round, what the choices
%% were  for the round and so on.    The supervisor starts arenas as "simple
%% one for one" type because they aren't  expected to be a "long lived
%% process." They exist for as long as a game goes on.   Once a winner has
%% been decided, it will shut down.

-module(throwdown_arena_sup).
-behaviour(supervisor).

-define(SERVER, ?MODULE).

-export([
         start_link/0,
         start_arena/1,
         init/1
        ]).

start_arena(Name) ->
    supervisor:start_child(?SERVER, [Name]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

arena_spec() ->
    #{id       => undefined,
      start    => {throwdown_arena, start_link, []},
      restart  => temporary,
      shutdown => 2000,
      type     => worker,
      modules  => [throwdown_arena]}.

init([]) ->
    SupFlags = #{ strategy => simple_one_for_one },
    Children = [ arena_spec() ],
    {ok, {SupFlags, Children}}.
