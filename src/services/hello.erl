%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 20:30
%%%-------------------------------------------------------------------
-module(hello).
-author("stefano.bertolotto").

-behavior(service).

%% API
-export([start_link/1]).
-export([message/3]).

start_link(Name) ->
    service:start_link(?MODULE, Name).

message(#{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"from">> := From, <<"entities">> := E} = Message}, BotName, _State) ->
    lager:warning("Entities: ~p", [E]),
    {<<"/start">>, BotName, true, _} = pe4kin_types:message_command(BotName, Message),
    FirstName = maps:get(<<"first_name">>, From, <<"Anonimous">>),
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<"Hello ", FirstName/binary>>}),
    ok;
message(Msg, _Name, _State) -> unhandled(Msg).

unhandled(Msg) ->
    lager:warning("Unhandled ~p ~p", [?FUNCTION_NAME, Msg]),
    ok.
