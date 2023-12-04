%%%-------------------------------------------------------------------
%% @doc roundrobin public API
%% @end
%%%-------------------------------------------------------------------

-module(roundrobin_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    roundrobin_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
