-module(throwdown_app).
-behaviour(application).

-export([
         start/2,
         stop/1
        ]).

start(_Type, _Args) ->
    throwdown_sup:start_link().

stop(_Args) -> ok.
