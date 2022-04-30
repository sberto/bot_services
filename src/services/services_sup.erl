%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2022 10:43
%%%-------------------------------------------------------------------
-module(services_sup).
-author("stefano.bertolotto").

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(M,F,A), #{id => M, start => {M, F, A}, restart => transient, type => worker}).
-define(SERVICE(M,A), ?CHILD(M,start_link,[BotName]++A)).
-define(SERVICE(M), ?SERVICE(M,[])).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(BotName :: binary()) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(BotName) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [BotName]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
    {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
                       MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
          [ChildSpec :: supervisor:child_spec()]}}
    | ignore | {error, Reason :: term()}).
init([BotName]) ->
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = #{strategy => one_for_one,
                 intensity => MaxRestarts,
                 period => MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?SERVICE(hello)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
