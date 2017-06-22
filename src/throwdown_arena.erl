-module(throwdown_arena).
-behaviour(gen_server).

-define(SERVER, ?MODULE).

-record(state, {
          rules :: function(),
          players = #{} :: map(),
          game_state = #{} :: map()
}).


-export([
         start_link/0,
         child_spec/0,
         register_player/2,
         evaluate_choices/2,
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

start_link() ->
    start_link(fun default_rules/2).

start_link(Rules) when is_function(Rules) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Rules], []).

child_spec() ->
    #{id       => throwdown_arena,
      start    => {throwdown_arena, start_link, []},
      restart  => temporary,
      shutdown => 2000,
      type     => worker,
      modules  => [throwdown_arena]}.

default_rules(rock, rock) -> false;
default_rules(rock, paper) -> false;
default_rules(rock, scissors) -> true;
default_rules(rock, lizard) -> true;
default_rules(rock, spock) -> false;

default_rules(paper, paper) -> false;
default_rules(paper, rock) -> true;
default_rules(paper, scissors) -> false;
default_rules(paper, lizard) -> false;
default_rules(paper, spock) -> true;

default_rules(scissors, scissors) -> false;
default_rules(scissors, rock) -> false;
default_rules(scissors, paper) -> true;
default_rules(scissors, lizard) -> true;
default_rules(scissors, spock) -> false;

default_rules(lizard, lizard) -> false;
default_rules(lizard, rock) -> false;
default_rules(lizard, paper) -> true;
default_rules(lizard, scissors) -> false;
default_rules(lizard, spock) -> true;

default_rules(spock, spock) -> false;
default_rules(spock, rock) -> true;
default_rules(spock, paper) -> false;
default_rules(spock, scissors) -> true;
default_rules(spock, lizard) -> false.

register_player(Name, PlayerPid) ->
    gen_server:call(?SERVER, {register, Name, PlayerPid}).

submit_choice(Name, Choice) ->
    gen_server:call(?SERVER, {choice, {Name, Choice}}).

%% gen_server callback

init([Rules]) ->
    Choices = throwdown:get_env(choices, [rock, paper, scissors, lizard, spock]),
    GState = #{ choices => Choices, current => ordsets, rounds => [] },
    {ok, #state{ rules = Rules, game_state = GState }}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_call({choice, C}, _From, State = #state{ players = P, game_state = G }) ->
    Current = maps:get(current, G),
    NewCurrent = ordset:add_element(C, Current),
    NewG = maps:put(current, NewCurrent, G),
    case ordset:size(NewCurrent) == length(maps:keys(P)) of
        true -> ?SERVER ! start_round;
        false -> ok
    end,
    {reply, ok, State#state{ game_state = NewG }};


handle_call({register, Name, PlayerPid}, _From, State = #state{ players = P }) ->
    NewP = maps:put(Name, PlayerPid, P),
    {reply, ok, State#state{ players = NewP }};

handle_call(_Call, _From, State) ->
    {reply, dieeeeee, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private

evaluate_choices(_Rules, [], Acc) -> Acc;
evaluate_choices(_Rules, P1, Acc) -> [ P1 | Acc ];
evaluate_choices(Rules, [ P1 = {_Name1, Choice1}, P2 = {_Name2, Choice2} | Rest ], Acc) ->
    case {Rules(Choice1, Choice2), Rules(Choice2, Choice1)} of
        {true, false} -> [ P1 | Acc ];
        {false, true} -> [ P2 | Acc ];
        {false, false} -> [ {tie, {P1, P2}} | Acc ];
        Other -> io:format(user, "Got ~p!! Weird~n", [Other])
    end,
    evaluate_choices(Rules, Rest, Acc).



-ifdef(TEST).

-endif.
