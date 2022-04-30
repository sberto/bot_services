%%%-------------------------------------------------------------------
%% @doc bot_services top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bot_services_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    {ok, BotName} = application:get_env(name),
    {ok, BotToken} = application:get_env(token),
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
                     #{id => bot, start => {pe4kin, launch_bot, [BotName, BotToken, #{receiver => true}]}},
                     #{id => services_sup, start => {services_sup, start_link, [BotName]}, type => supervisor}
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
