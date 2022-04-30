%%%-------------------------------------------------------------------
%% @doc bot_services public API
%% @end
%%%-------------------------------------------------------------------

-module(bot_services_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    application:ensure_all_started(pe4kin),
    bot_services_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
