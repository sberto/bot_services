%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 20:30
%%%-------------------------------------------------------------------
-module(hello_service).
-author("stefano.bertolotto").

-include("bot_types.hrl").
-include("services.hrl").
-behavior(service).

%% API
-export([start_link/1]).
%% service callbacks
-export([command/4, message/3]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name) ->
    service:start_link(?MODULE, Name).

%%%===================================================================
%%% service callbacks
%%%===================================================================
-spec command(command(), ?CALLBACK_ARGS) -> ?CALLBACK_RES.
command(<<"/start">>, #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"from">> := From}}, BotName, _State) ->
    send_hello(BotName, ChatId, From),
    ok;
command(Cmd, #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}}}, BotName, _State) ->
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<Cmd/binary, " is not a valid command">>}),
    ok;
command(_Cmd, Msg, _Name, _State) -> unhandled(Msg).

message(#{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"from">> := From}}, BotName, _State) ->
    send_hello(BotName, ChatId, From),
    ok;
message(Msg, _Name, _State) -> unhandled(Msg).

%%%===================================================================
%%% Internal functions
%%%===================================================================
unhandled(Msg) ->
    lager:warning("Unhandled ~p ~p", [?FUNCTION_NAME, Msg]),
    ok.

send_hello(BotName, ChatId, From) ->
    FirstName = maps:get(<<"first_name">>, From, <<"Anonimous">>),
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<"Hello ", FirstName/binary>>}).