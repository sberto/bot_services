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

-behaviour(gen_server).
-include("bot_types.hrl").
-include("services.hrl").

-callback message(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback command(command(), command_type(), ?CALLBACK_ARGS) -> ?CALLBACK_RES.
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

-optional_callbacks([message/3, command/5, other_bot_command/5, edited_message/3, channel_post/3, edited_channel_post/3, inline_query/3, chosen_inline_result/3, callback_query/3, shipping_query/3, pre_checkout_query/3, undefined/3]).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(UPDATE, pe4kin_update).
-define(SERVER, ?MODULE).

-record(state, {name, mod, state = #{}}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(module(), bot_name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Mod, BotName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Mod, BotName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: list()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Mod, BotName]) ->
    pe4kin_receiver:subscribe(BotName, self()),
    pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
    lager:notice("[~p] ~p init'd", [?MODULE, Mod]),
    {ok, #state{name = BotName, mod = Mod}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #state{}) ->
                     {reply, Reply :: term(), NewState :: #state{}} |
                     {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
                     {noreply, NewState :: #state{}} |
                     {noreply, NewState :: #state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
                     {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Request, From, State = #state{name = Name}) ->
    io:format("Unhandled ~p req ~p from ~p, name ~p", [?FUNCTION_NAME, Request, From, Name]),
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(Request, State = #state{name = Name}) ->
    io:format("Unhandled ~p req: ~p, name ~p", [?FUNCTION_NAME, Request, Name]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({?UPDATE, BotName, Msg},
            State =
                #state{
                    name  = BotName,
                    mod   = Mod,
                    state = ServiceState
                }) ->
    {Fun, AdditionalArgs} = callback(BotName, Msg),
    Args = AdditionalArgs ++ [Msg, BotName, ServiceState],
    lager:info("[~p] -> ~p:~p", [?MODULE, Mod, Fun]),
    case maybe_apply(Mod, Fun, Args) of
        {ok, NewServiceState} -> {noreply, State#state{state = NewServiceState}};
        ok -> {noreply, State};
        {error, not_exported} -> unhandled(Args)
    end;
handle_info(Info, State = #state{name = Name}) ->
    io:format("Unhandled ~p req: ~p name ~p", [?FUNCTION_NAME, Info, Name]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State = #state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
                     {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unhandled(Msg) ->
    lager:info("Unhandled ~p ~p", [?FUNCTION_NAME, Msg]),
    ok.

maybe_apply(M, F, A) ->
    IsExported = erlang:function_exported(M, F, length(A)),
    case IsExported of
        true -> apply(M, F, A);
        false -> {error, not_exported}
    end.


-spec callback(BotName :: bot_name(), Msg :: map()) -> {F :: atom(), AdditionalArgs :: list()}.
callback(BotName, #{<<"message">> := #{<<"entities">> := _} = Msg}) -> command_callback(pe4kin_types:message_command(BotName, Msg));
callback(_, Msg) -> {pe4kin_types:update_type(Msg), []}.

command_callback({Cmd, BinBotName, true, _Command}) -> {command, [Cmd, tagged]}; % /cmd@this_bot
command_callback({Cmd, OtherBotName, false, _Command}) -> {other_bot_command, [Cmd, OtherBotName]}; % /cmd@other_bot
command_callback({Cmd, _BinBotName, true, _Command}) -> {command, [Cmd, direct]}. % /cmd