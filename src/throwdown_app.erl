%% @doc This is the main OTP application callback module.  It doesn't do much
%% more than start the main supervisor.

-module(throwdown_app).
-behaviour(application).

-export([
         start/2,
         stop/1
        ]).

start(_Type, _Args) ->
    throwdown_sup:start_link().

stop(_Args) -> ok.
