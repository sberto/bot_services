%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 19:35
%%%-------------------------------------------------------------------
-module(service).
-author("stefano.bertolotto").

-behaviour(srv).
-include("bot_types.hrl").
-include("services.hrl").

-callback message(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback command(command(), ?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback other_bot_command(command(), OtherBotName :: bot_name(), ?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback edited_message(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback channel_post(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback edited_channel_post(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback inline_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback chosen_inline_result(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback callback_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback shipping_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback pre_checkout_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback undefined(?CALLBACK_ARGS) -> ?CALLBACK_RES.

-optional_callbacks([message/3, command/4, other_bot_command/5, edited_message/3, channel_post/3, edited_channel_post/3, inline_query/3, chosen_inline_result/3, callback_query/3, shipping_query/3, pre_checkout_query/3, undefined/3]).

%% API
-export([start_link/2]).

%% srv callbacks
-export([handle_info/5]).

-define(SERVER, ?MODULE).

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

handle_info(BotName, Type, AdditionalArgs, Msg,
            State =
                #srv_state{
                    name    = BotName,
                    mod     = Mod,
                    state   = ServiceState
                }) ->
    Args = AdditionalArgs ++ [Msg, BotName, ServiceState],
    lager:debug("[~p] Args ~p~n", [?MODULE, Args]),
    case maybe_apply(Mod, Type, Args) of
        {ok, NewServiceState} -> {noreply, State#srv_state{state = NewServiceState}};
        ok -> {noreply, State};
        {error, not_exported} -> unhandled(Args, State)
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