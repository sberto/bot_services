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

-behavior(bot_service).

%% API
-export([start_link/1]).
-export([message/3,
         edited_message/3,
         channel_post/3,
         edited_channel_post/3,
         inline_query/3,
         chosen_inline_result/3,
         callback_query/3,
         shipping_query/3,
         pre_checkout_query/3]).

start_link(Name) ->
    bot_service:start_link(?MODULE, Name).

message(#{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"from">> := From} = Message}, BotName, State) ->
    % {<<"/start">>, BotName, true, _} = pe4kin_types:message_command(BotName, Message),
    % FirstName = maps:get(<<"first_name">>, From, <<"Anonimous">>),
    % {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<"Ciao ", FirstName/binary>>}),
    pe4kin:send_message(BotName, #{chat_id => ChatId, text => <<"Ciao!">>}),
    ok.

edited_message(_Msg, _Name, _State) -> ok.
channel_post(_Msg, _Name, _State) -> ok.
edited_channel_post(_Msg, _Name, _State) -> ok.
inline_query(_Msg, _Name, _State) -> ok.
chosen_inline_result(_Msg, _Name, _State) -> ok.
callback_query(_Msg, _Name, _State) -> ok.
shipping_query(_Msg, _Name, _State) -> ok.
pre_checkout_query(_Msg, _Name, _State) -> ok.