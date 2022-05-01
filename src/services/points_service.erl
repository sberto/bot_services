%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 20:30
%%%-------------------------------------------------------------------
-module(points_service).
-author("stefano.bertolotto").

-include("bot_types.hrl").
-include("services.hrl").
-behavior(service).

%% API
-export([start_link/1]).
%% service callbacks
-export([command/4, message/3]).

-record(points_state, {next, teams, accumulator = []}).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name) ->
    service:start_link(?MODULE, Name).

%%%===================================================================
%%% service callbacks
%%%===================================================================
-spec command(command(), ?CALLBACK_ARGS) -> ?CALLBACK_RES.
command(<<"/start">>, Msg, BotName, State) -> start(chat_id(Msg), BotName, State);
command(<<"/points">>, Msg, BotName, State) -> points(chat_id(Msg), BotName, State);
command(<<"/done">>, Msg, BotName, State)   -> done(chat_id(Msg), BotName, State);
command(_Cmd, Msg, _Name, State)           -> unhandled(?FUNCTION_NAME, Msg, State).

message(#{<<"message">> := #{<<"text">> := Team}}, _BotName, State = #points_state{next = team_names, accumulator = Acc}) ->
    NewState = State#points_state{accumulator = [Team | Acc]},
    {ok, NewState};
message(Msg, _Name, State) ->
    unhandled(?FUNCTION_NAME, Msg, State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Commands
start(ChatId, BotName, #points_state{}) ->
    msg(<<"/points: record scores in a game">>, ChatId, BotName),
    ok;
start(ChatId, BotName, _) ->
    msg(<<"/points: record scores in a game">>, ChatId, BotName),
    lager:info("Inited ~p state", [?MODULE]),
    {ok, #points_state{}}.

points(ChatId, BotName, State) ->
    msg(<<"How many team? Write /done when you are done">>, ChatId, BotName),
    {ok, State#points_state{next = team_names}}.

done(ChatId, BotName, State = #points_state{next = team_names, accumulator = Acc}) ->
    Teams = lists:reverse(Acc),
    msg(<<"Your teams are: ", (list(Teams))/binary>>, ChatId, BotName),
    {ok, State#points_state{next = undefined, teams = Teams}}.

%%% Internal functions
unhandled(Fun, Msg, State) ->
    lager:warning("Unhandled ~p ~p with state ~p", [Fun, Msg, State]),
    ok.

msg(Msg, ChatId, BotName) ->
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Msg}).

chat_id(Msg) -> {ok, ChatId} = pe4kin_types:chat_id(message, Msg), ChatId.

list(List) ->
    lists:foldl(
        fun(E, Acc) ->
            <<Acc/binary, "\n- ", E/binary>>
        end,
        <<"">>,
        List
    ).