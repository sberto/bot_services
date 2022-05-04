%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 19:35
%%%-------------------------------------------------------------------
-module(service_fsm).
-author("stefano.bertolotto").

-behaviour(srv).
-include("bot_types.hrl").
-include("services.hrl").

%% API
-export([start_link/2, init/2]).

%% srv callbacks
-export([handle_info/5]).

-define(SERVER, ?MODULE).

-callback init(BotName :: bot_name()) -> {ok, FsmState :: atom(), State :: any()} | {ok, State :: atom()}.

-callback generic(Type :: atom(), AddArgs :: list(), Msg :: msg(), Name :: bot_name(), State :: map()) ->
    {new_state, FsmState :: atom()} |
    {new_state, FsmState :: atom(), State :: map()} |
    {ok, State :: map()} |
    ok.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(module(), bot_name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Mod, BotName) ->
    srv:start_link(?MODULE, Mod, BotName).

%%%===================================================================
%%% srv callbacks
%%%===================================================================

-spec init(Mod :: atom(), BotName :: bot_name()) -> {ok, SrvState :: map(), State :: any()}.
init(Mod, BotName) ->
    case Mod:init(BotName) of
        {ok, State} -> {ok, #{state => start}, State};
        {ok, FsmState, State} -> {ok, #{state => FsmState}, State}
    end.

handle_info(BotName, Type, AdditionalArgs, Msg,
            State =
                #srv_state{
                    name      = BotName,
                    mod       = Mod,
                    srv_state = SrvState,
                    state     = ServiceState
                }) ->
    Args = [Type] ++ [AdditionalArgs] ++ [Msg, BotName, ServiceState],
    maybe_apply(Mod, generic, Args),
    lager:debug("[~p] Args ~p~n", [?MODULE, Args]),
    FsmState = maps:get(state, SrvState),
    case maybe_apply(Mod, FsmState, Args) of
        {new_state, NewFsmState} when is_atom(NewFsmState) ->
            {noreply, State#srv_state{srv_state = #{state => NewFsmState}}};
        {new_state, NewFsmState, NewServiceState} when is_atom(NewFsmState) ->
            NewState =
                State#srv_state{
                    state     = NewServiceState,
                    srv_state = #{state => NewFsmState}
                },
            {noreply, NewState};
        {ok, NewServiceState} ->
            {noreply, State#srv_state{state = NewServiceState}};
        ok ->
            {noreply, State};
        {error, not_exported} ->
            unhandled(Args, State)
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

unhandled(Msg, State) ->
    lager:info("Unhandled ~p", [Msg]),
    {noreply, State}.

maybe_apply(M, F, A) ->
    IsExported = erlang:function_exported(M, F, length(A)),
    case IsExported of
        true -> apply(M, F, A);
        false -> {error, not_exported}
    end.