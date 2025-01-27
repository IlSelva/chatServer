%%%-------------------------------------------------------------------
%% @doc chatServer public API
%% @end
%%%-------------------------------------------------------------------

-module(chatServer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chatServer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
