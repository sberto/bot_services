%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 19:35
%%%-------------------------------------------------------------------
-module(bot_service).
-author("stefano.bertolotto").

-behaviour(gen_server).
-include("bot_types.hrl").

-define(CALLBACK_ARGS, Msg :: map(), Name :: bot_name(), ServiceState :: map()).
-define(CALLBACK_RES, ok | {ok, NewState :: map()}).

-callback message(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback edited_message(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback channel_post(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback edited_channel_post(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback inline_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback chosen_inline_result(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback callback_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback shipping_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback pre_checkout_query(?CALLBACK_ARGS) -> ?CALLBACK_RES.
-callback undefined(?CALLBACK_ARGS) -> ?CALLBACK_RES.

-optional_callbacks([undefined/3]).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(UPDATE, pe4kin_update).
-define(SERVER, ?MODULE).

-record(bot_service_state, {name, mod, service_state = #{}}).

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
    {ok, State :: #bot_service_state{}} | {ok, State :: #bot_service_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Mod, BotName]) ->
    pe4kin_receiver:subscribe(BotName, self()),
    pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
    lager:notice("[~p] ~p init'd", [?MODULE, Mod]),
    {ok, #bot_service_state{name = BotName, mod = Mod}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #bot_service_state{}) ->
                     {reply, Reply :: term(), NewState :: #bot_service_state{}} |
                     {reply, Reply :: term(), NewState :: #bot_service_state{}, timeout() | hibernate} |
                     {noreply, NewState :: #bot_service_state{}} |
                     {noreply, NewState :: #bot_service_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #bot_service_state{}} |
                     {stop, Reason :: term(), NewState :: #bot_service_state{}}).
handle_call(Request, From, State = #bot_service_state{name = Name}) ->
    io:format("Unhandled ~p req ~p from ~p, name ~p", [?FUNCTION_NAME, Request, From, Name]),
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #bot_service_state{}) ->
    {noreply, NewState :: #bot_service_state{}} |
    {noreply, NewState :: #bot_service_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #bot_service_state{}}).
handle_cast(Request, State = #bot_service_state{name = Name}) ->
    io:format("Unhandled ~p req: ~p, name ~p", [?FUNCTION_NAME, Request, Name]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #bot_service_state{}) ->
    {noreply, NewState :: #bot_service_state{}} |
    {noreply, NewState :: #bot_service_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #bot_service_state{}}).
handle_info({?UPDATE, BotName, Msg},
            State =
                #bot_service_state{
                    name          = BotName,
                    mod           = Mod,
                    service_state = ServiceState
                }) ->
    Type = pe4kin_types:update_type(Msg),
    Fun = Type,
    lager:notice("[~p] ~p -> ~p:~p", [?MODULE, Type, Mod, Fun]),
    case Mod:Type(Msg, BotName, ServiceState) of
        {ok, NewServiceState} -> {noreply, State#bot_service_state{service_state = NewServiceState}};
        ok -> {noreply, State}
    end;
handle_info(Info, State = #bot_service_state{name = Name}) ->
    io:format("Unhandled ~p req: ~p name ~p", [?FUNCTION_NAME, Info, Name]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #bot_service_state{}) -> term()).
terminate(_Reason, _State = #bot_service_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #bot_service_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #bot_service_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #bot_service_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

