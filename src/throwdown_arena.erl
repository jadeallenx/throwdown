%% @doc This module represents an arena process. The arena is where throwdown
%% games are executed. The arena process maintains the following pieces of
%% state:
%%
%% <ul>
%% <li>`mode': An atom describing what phase the game is in. This is help
%% isolate and identify API calls which may not be legal at certain times.</li>
%% <li>`name': A name for the arena. Can be an atom, string, or binary
%% string.</li>
%% <li>`rules': A function which decides which moves beat other moves. The
%% `default_rules/2' function implements the rules for Sam Kass' "Rock, paper,
%% scissors, lizard, spock" as described in the <a
%% href="http://www.samkass.com/theories/RPSSL.html">original web
%% page</a>.</li>
%% <li>`players': A map of player names to pids representing those
%% players.</li>
%% <li>`game_state': The current game state. See below for more details about
%% game state.</li>
%% </ul>
%%
%% Arenas exit after a winner has been decided or all players have been
%% eliminated.
%%
%% <h3>Game state</h3>
%% Game state is represented as a map with the following keys:
%% <ul>
%% <li>`choices': These are the valid, legal moves that are available. Can be
%% modified by setting the application environment variable `choices'. By
%% default this is rock, paper, scissors, lizard, spock. This list can be
%% used by player processes to select a valid legal move.</li>
%% <li>`current': A set representing the moves in the current round of the
%% game.</li>
%% <li>`results': A list representing the past moves and outcomes from past
%% rounds of this game.</li>
%% </ul>
%%
%% This game state is passed into the player process so that each player may
%% make a move selection.

-module(throwdown_arena).
-behaviour(gen_server).

-type arena_name() :: atom() | string() | binary().
-type arena_mode() :: 'stop' | 'waiting' | 'playing' | 'evaluation'.
-type move() :: 'rock' | 'paper' | 'scissors' | 'lizard' | 'spock'.

-record(state, {
          name             :: arena_name(),
          mode             :: arena_mode(),
          rules            :: function(),
          players = #{}    :: map(),
          game_state = #{} :: map()
}).


-export([
         start_link/1,
         start_link/2,
         child_spec/1,
         register_player/3,
         submit_choice/3,
         done/1,
         default_rules/2
        ]).

-export([
         init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% public API

-spec start_link( Name :: arena_name() ) -> {ok, Pid :: pid()}.
%% @doc Start an arena with the given name using the default rules.
%% This is the normal way arenas are created.
start_link(Name) ->
    start_link(Name, fun default_rules/2).

-spec start_link( Name :: arena_name(),
                  Rules :: function() ) -> {ok, Pid :: pid()}.
%% @doc Start an arena with the given name and a rules function.
%% The rules function should take two moves and determine one of the following
%% atoms: `win', `loss', `tie' depending on whether the first move beats the
%% second move.
start_link(Name, Rules) when is_function(Rules) ->
    gen_server:start_link(?MODULE, [Name, Rules], []).

%% @private Convenience function for the supervisor.
child_spec(Name) ->
    #{id       => Name,
      start    => {throwdown_arena, start_link, []},
      restart  => temporary,
      shutdown => 2000,
      type     => worker,
      modules  => [throwdown_arena]}.

-spec default_rules( MoveA :: move(),
                     MoveB :: move() ) -> 'win' | 'loss' | 'tie'.
%% @doc The default set of rules to implement rock, paper, scissors, lizard,
%% spock.
default_rules(rock, rock) -> tie;
default_rules(rock, paper) -> loss;
default_rules(rock, scissors) -> win;
default_rules(rock, lizard) -> win;
default_rules(rock, spock) -> loss;

default_rules(paper, paper) -> tie;
default_rules(paper, rock) -> win;
default_rules(paper, scissors) -> loss;
default_rules(paper, lizard) -> loss;
default_rules(paper, spock) -> win;

default_rules(scissors, scissors) -> tie;
default_rules(scissors, rock) -> loss;
default_rules(scissors, paper) -> win;
default_rules(scissors, lizard) -> win;
default_rules(scissors, spock) -> loss;

default_rules(lizard, lizard) -> tie;
default_rules(lizard, rock) -> loss;
default_rules(lizard, paper) -> win;
default_rules(lizard, scissors) -> loss;
default_rules(lizard, spock) -> win;

default_rules(spock, spock) -> tie;
default_rules(spock, rock) -> win;
default_rules(spock, paper) -> loss;
default_rules(spock, scissors) -> win;
default_rules(spock, lizard) -> loss.

-spec register_player( Arena :: pid(),
                       Name :: binary(),
                       PlayerPid :: pid() ) -> ok | {error, cannot_register}.
%% @doc This API call registers a player process for a game in this arena.
register_player(Arena, Name, PlayerPid) ->
    gen_server:call(Arena, {register, Name, PlayerPid}).

-spec submit_choice( Arena :: pid(),
                     Name :: arena_name(),
                     Pick :: move() ) -> ok | {error, cannot_select}.
%% @doc This API call submits a player move to the current round of the game.
submit_choice(Arena, Name, Pick) ->
    gen_server:call(Arena, {choice, {Name, Pick}}).

-spec done( Arena :: pid() ) -> ok.
%% @doc Signal the arena that all the players have been added and that playing
%% rounds should begin.
done(Arena) ->
    gen_server:call(Arena, done).

%% gen_server callback
%% @private
init([Name, Rules]) ->
    Choices = throwdown:get_env(choices, [rock, paper, scissors, lizard, spock]),
    GState = #{ choices => Choices, current => ordsets:new(), results => [] },
    {ok, #state{ mode = waiting, name = Name, rules = Rules, game_state = GState }}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_call(done, _From, State = #state{ players = P, game_state = G } ) ->
    {Reply, NewState} = case maps:size(P) of
        X when X < 2  ->
            {{error, not_enough_players}, State};
        _ ->
            play_round(P, G),
            {ok, State#state{mode = playing}}
    end,
    {reply, Reply, NewState};
handle_call({choice, _C}, _From, State = #state{ mode = waiting }) ->
    {reply, {error, cannot_select}, State};
handle_call({choice, C}, _From, State = #state{ mode = playing, players = P, game_state = G }) ->
    Current = maps:get(current, G),
    NewCurrent = ordsets:add_element(C, Current),
    NewG = maps:put(current, NewCurrent, G),
    NewMode = case ordsets:size(NewCurrent) == maps:size(P) of
        true ->
            self() ! start_round,
            evaluation;
        false ->
            playing
    end,
    {reply, ok, State#state{ mode = NewMode, game_state = NewG }};

handle_call({register, _Name, _PlayerPid}, _From, State = #state{ mode = playing }) ->
    {reply, {error, cannot_register}, State};
handle_call({register, Name, PlayerPid}, _From, State = #state{ mode = waiting,
                                                                players = P }) ->
    NewP = maps:put(Name, PlayerPid, P),
    {reply, ok, State#state{ players = NewP }};

handle_call(_Call, _From, State) ->
    {reply, dieeeeee, State}.

handle_info(start_round, State = #state{ mode = evaluation, rules = R, players = P, game_state = G }) ->
    Current = ordsets:to_list(maps:get(current, G)),
    Results = evaluate_choices(R, Current, Current, []),
    G1 = maps:put(current, ordsets:new(), G),
    R0 = maps:get(results, G),
    NewG = maps:put(results, [ Current | R0 ], G1),
    NewP = remove_players(Results, P),
    case maps:size(NewP) of
        0 ->
            {stop, no_players_remain, State#state{ mode = stop, players = NewP, game_state = NewG}};
        1 ->
            {stop, {winner, NewP}, State#state{ mode = stop, players = NewP, game_state = NewG}};
        _ ->
            play_round(NewP, NewG),
            {noreply, State#state{ mode = playing, players = NewP, game_state = NewG }}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private

%% @private For every player that's registered, ask that process to make a new
%% play with the current game state.
play_round(P, G) ->
    maps:map(fun(_Name, Pid) ->
                     throwdown_player:play(Pid, G)
             end,
             P).


%% @private After a round has been completed. Remove players from the arena
%% who lost.
remove_players([], P) -> P;
remove_players([ {loss, {Name, _Pick}} | Tail], P) ->
    PlayPid = maps:get(Name, P),
    throwdown_player:leave(PlayPid),
    remove_players(Tail, maps:remove(Name, P));
remove_players([ _H | T ], P) ->
    remove_players(T, P).

%% @private Fold over the set of choices and determine winners, ties and
%% losers.
evaluate_choices(_Rules, [], _Picks, Acc) -> Acc;
evaluate_choices(Rules, [ H | Rest ], All, Acc) ->
    Picks = All -- [H],
    Outcome = case versus(Rules, H, Picks, undefined) of
        tie -> {tie, H};
        win -> {win, H};
        loss -> {loss, H}
    end,
    evaluate_choices(Rules, Rest, All, [ Outcome | Acc ]).

%% @private This is the function which directly evaluates a player against
%% all other player moves in the current round.  If the result is a loss,
%% return immediately.
versus(_Rules, _Player, [], Result) -> Result;
versus(Rules, {_NameA, PlayA} = A, [ {_NameB, PlayB} | T ], _LastResult) ->
    case Rules(PlayA, PlayB) of
        loss ->
            loss;
        Result ->
            versus(Rules, A, T, Result)
    end.
