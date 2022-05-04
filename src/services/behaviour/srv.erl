%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 19:35
%%%-------------------------------------------------------------------
-module(srv).
-author("stefano.bertolotto").

-behaviour(gen_server).
-include("bot_types.hrl").
-include("services.hrl").

-callback handle_info(BotName :: bot_name(), Type :: atom(), AdditionalArgs :: list(), Msg :: msg(), State :: #srv_state{}) -> {noreply, State :: #srv_state{}}.
-callback init(Mod :: atom(), BotName :: bot_name()) -> {ok, SrvState :: any(), State :: any()}.

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(UPDATE, pe4kin_update).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link(module(), module(), bot_name()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(SrvMod, Mod, BotName) ->
    gen_server:start_link({local, Mod}, ?MODULE, [SrvMod, Mod, BotName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: list()) ->
    {ok, State :: #srv_state{}} | {ok, State :: #srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([SrvMod, Mod, BotName]) ->
    pe4kin_receiver:subscribe(BotName, self()),
    pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
    lager:notice("[~p] ~p init'd", [?MODULE, Mod]),
    {ok, SrvState, State} = SrvMod:init(Mod, BotName),
    {ok, #srv_state{name = BotName, mod = Mod, srv_mod = SrvMod, srv_state = SrvState, state = State}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
                  State :: #srv_state{}) ->
                     {reply, Reply :: term(), NewState :: #srv_state{}} |
                     {reply, Reply :: term(), NewState :: #srv_state{}, timeout() | hibernate} |
                     {noreply, NewState :: #srv_state{}} |
                     {noreply, NewState :: #srv_state{}, timeout() | hibernate} |
                     {stop, Reason :: term(), Reply :: term(), NewState :: #srv_state{}} |
                     {stop, Reason :: term(), NewState :: #srv_state{}}).
handle_call(Request, From, State = #srv_state{name = Name}) ->
    io:format("Unhandled ~p req ~p from ~p, name ~p", [?FUNCTION_NAME, Request, From, Name]),
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #srv_state{}) ->
    {noreply, NewState :: #srv_state{}} |
    {noreply, NewState :: #srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #srv_state{}}).
handle_cast(Request, State = #srv_state{name = Name}) ->
    io:format("Unhandled ~p req: ~p, name ~p", [?FUNCTION_NAME, Request, Name]),
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #srv_state{}) ->
    {noreply, NewState :: #srv_state{}} |
    {noreply, NewState :: #srv_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #srv_state{}}).
handle_info({?UPDATE, BotName, Msg},
            State =
                #srv_state{
                    name    = BotName,
                    srv_mod = SrvMod,
                    mod     = Mod,
                    state   = _ServiceState
                }) ->
    lager:debug("[~p] Msg ~n~p~n", [?MODULE, Msg]),
    {Type, AdditionalArgs} = callback(BotName, Msg),
    lager:info("[~p] -> ~p:~p", [?MODULE, Mod, Type]),
    SrvMod:handle_info(BotName, Type, AdditionalArgs, Msg, State);
handle_info(Info, State = #srv_state{name = Name}) ->
    io:format("Unhandled ~p req: ~p name ~p", [?FUNCTION_NAME, Info, Name]),
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #srv_state{}) -> term()).
terminate(_Reason, _State = #srv_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #srv_state{},
                  Extra :: term()) ->
                     {ok, NewState :: #srv_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #srv_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec callback(BotName :: bot_name(), Msg :: map()) -> {F :: atom(), AdditionalArgs :: list()}.
callback(BotName, #{<<"message">> := #{<<"entities">> := _} = Msg}) ->
    command_callback(pe4kin_types:message_command(BotName, Msg));
callback(_, Msg) ->
    {pe4kin_types:update_type(Msg), []}.

command_callback({Cmd, _BinBotName, true, _Cmd})       -> {command, [Cmd]}; % /cmd
command_callback({Cmd, OtherBotName, false, _Command}) -> {other_bot_command, [Cmd, OtherBotName]}. % /cmd@other_bot
