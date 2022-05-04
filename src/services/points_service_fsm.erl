%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Apr 2022 20:30
%%%-------------------------------------------------------------------
-module(points_service_fsm).
-author("stefano.bertolotto").

-include("bot_types.hrl").
-include("behaviour/services.hrl").
-behavior(service_fsm).

%% API
-export([start_link/1]).
%% service callbacks
-export([init/1, starting_state/5, team_names/5, generic/5, points/5]).

%% Definitions
-define(SV_POINTS, points).
-define(SV_TEAMS_TO_UPDATE, teams_to_update).
-define(STATE_VALUES,
    [
        ?SV_POINTS,
        ?SV_TEAMS_TO_UPDATE
    ]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name) ->
    service_fsm:start_link(?MODULE, Name).

%%%===================================================================
%%% service_fsm callbacks
%%%===================================================================

-spec init(BotName :: bot_name()) -> {ok, SrvState :: map(), State :: any()}.
init(_) ->
    {ok, starting_state, #{}}.

-spec generic(Type :: atom(), AddArgs :: list(), Msg :: msg(), Name :: bot_name(), State :: map()) ->
    {new_state, FsmState :: atom()} |
    {new_state, FsmState :: atom(), State :: map()} |
    {ok, State :: map()} |
    ok.
generic(command, [<<"/dbg">>], Msg, BotName, State) -> dbg(chat_id(Msg), BotName, State);
generic(_, _, _, _, _)                              -> ok.

starting_state(command, [<<"/start">>], Msg, BotName, State) -> welcome(chat_id(Msg), BotName, State);
starting_state(T, A, M, N, S)                                -> unhandled(?FUNCTION_NAME, T, A, M, N, S).

team_names(command, [<<"/points">>], Msg, BotName, State) -> ask_team_names(chat_id(Msg), BotName, State);
team_names(command, [<<"/done">>], Msg, BotName, State)   -> clean(done(chat_id(Msg), BotName, State), State);
team_names(message, [], Msg, _, State)                    -> team_names_msg(Msg, State);
team_names(T, A, M, N, S)                                 -> unhandled(?FUNCTION_NAME, T, A, M, N, S).

points(command, [<<"/points">>], Msg, BotName, State)      -> points(chat_id(Msg), BotName, State);
points(command, [<<"/reset_teams">>], Msg, BotName, State) -> reset_teams(chat_id(Msg), BotName, State);
points(message, [], Msg, Name, State)                      -> points_msg(Msg, Name, State);
points(T, A, M, N, S)                                      -> unhandled(?FUNCTION_NAME, T, A, M, N, S).

%%%===================================================================
%%% Commands
%%%===================================================================

%%% Commands
welcome(ChatId, BotName, #{}) ->
    Welcome =
        <<"/points: shows points in a game\n"
          "/reset_teams: start over">>,
    msg(Welcome, ChatId, BotName),
    {new_state, team_names}.

ask_team_names(ChatId, BotName, State) ->
    msg(<<"What are your teams' names? Write /done when you are done">>, ChatId, BotName),
    {ok, State#{accumulator => []}}.

reset_teams(ChatId, BotName, _) ->
    msg(<<"Ok!">>, ChatId, BotName),
    {team_names, #{}}.

points(ChatId, BotName, #{?SV_POINTS := Points}) ->
    msg(<<"Your points are:", (list(maps:to_list(Points)))/binary>>, ChatId, BotName),
    ok.

done(ChatId, BotName, State = #{accumulator := Acc}) ->
    Teams = lists:reverse(Acc),
    Msg1 = <<"Your teams are: ", (list(Teams))/binary>>,
    Msg2 =
        <<"Write your points following the team order, "
          "otherwise you can write points like \"13W\" if the team \"Willy\" has scored 13 points."
          "I'll tell you the scores after you finished entering them.">>,
    msg(Msg1, ChatId, BotName),
    msg(Msg2, ChatId, BotName),
    NewState =
        State#{
            ?SV_POINTS => points_from_list(Teams),
            ?SV_TEAMS_TO_UPDATE => Teams
        },
    {new_state, points, NewState};
done(ChatId, BotName, State) -> welcome(ChatId, BotName, State).

dbg(ChatId, BotName, State) ->
    msg(jiffy:encode(State), ChatId, BotName),
    ok.

%%%===================================================================
%%% Messages
%%%===================================================================

team_names_msg(#{<<"message">> := #{<<"text">> := Team}}, State) ->
    Acc = maps:get(accumulator, State, []),
    NewState = State#{accumulator => [Team | Acc]},
    {ok, NewState}.

points_msg(Msg = #{<<"message">> := #{<<"text">> := Text}},
           BotName,
           State = #{?SV_POINTS := Points, ?SV_TEAMS_TO_UPDATE := MissingTeams}) ->
    case convert_to_points(Text, MissingTeams) of
        {ok, Delta, NewMissingTeams} ->
            NewState =
                State#{
                    ?SV_POINTS => update_points(Delta, Points),
                    ?SV_TEAMS_TO_UPDATE => teams_to_update(NewMissingTeams, Points)
                },
            maybe_show_results(NewMissingTeams, Msg, BotName, NewState),
            {ok, NewState};
        {error, {not_recognized, Unknown}} ->
            msg(<<"I don't recognize the team ", Unknown/binary>>, chat_id(Msg), BotName),
            ok;
        {error, {multiple_matches, Term}} ->
            msg(<<"Multiple teams match ", Term/binary>>, chat_id(Msg), BotName),
            ok
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

unhandled(Fun, Type, Additional, Msg, Name, State) ->
    lager:warning("Unhandled ~p~p with state ~p", [Fun, [Type, Additional, Msg, Name], State]),
    ok.

msg(Msg, ChatId, BotName) ->
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => Msg}).

chat_id(Msg) -> {ok, ChatId} = pe4kin_types:chat_id(message, Msg), ChatId.

list(List) ->
    lists:foldl(
        fun
            ({K, V}, Acc) -> <<Acc/binary, "\n- ", K/binary, ": ", (to_binary(V))/binary>>;
            (E, Acc) -> <<Acc/binary, "\n- ", E/binary>>
        end,
        <<"">>,
        List
    ).

points_from_list(Teams) ->
    lists:foldl(
        fun(T, Acc) -> Acc#{T => 0} end,
        #{},
        Teams
    ).

clean(ok, State)                  -> {ok, do_clean(State)};
clean({ok, State}, _)             -> {ok, do_clean(State)};
clean({new_state, Fsm, State}, _) -> {new_state, Fsm, do_clean(State)};
clean({new_state, Fsm}, State)    -> {new_state, Fsm, do_clean(State)}.

do_clean(State) ->
    maps:with(?STATE_VALUES, State).

to_binary(Term) when is_binary(Term) -> Term;
to_binary(Term)                      -> jiffy:encode(Term).

-spec convert_to_points(Text :: binary(), list()) -> {ok, DeltaMap :: map()} | {error, {not_recognized, Name :: binary()} | {error, {multiple_matches, Name :: binary()}}}.
convert_to_points(Text, MissingTeams) ->
    TeamPoints = services_utils:parse_num_words(Text),
    team_points_to_map(TeamPoints, MissingTeams).

-spec team_points_to_map(TeamPoints :: list(), list()) -> {ok, DeltaMap :: map()} | {error, {not_recognized, Name :: binary()} | {error, {multiple_matches, Name :: binary()}}}.
team_points_to_map(TeamPoints, MissingTeams) ->
    FindStarting =
        fun(Name, List) ->
            lists:filter(
                fun(Bin) -> binary:part(Bin, {0, size(Name)}) == Name end,
                List
            )
        end,
    lists:foldl(
        fun
            (_, {error, _} = Acc) -> Acc;
            ({Num, TeamQuery}, {ok, Map, Teams}) ->
                case FindStarting(TeamQuery, Teams) of
                    [Team] -> {ok, Map#{Team => Num}, Teams -- [Team]};
                    [] -> {error, {not_recognized, TeamQuery}};
                    _ -> {error, {multiple_matches, TeamQuery}}
                end;
            (Num, {ok, Map, [Team | Teams]}) ->
                {ok, Map#{Team => Num}, Teams}
        end,
        {ok, #{}, MissingTeams},
        TeamPoints
    ).

update_points(Delta, Points) ->
    maps:map(
        fun(K, V) -> maps:get(K, Delta, 0) + V end,
        Points
    ).

teams_to_update([], Points)               -> maps:keys(Points);
teams_to_update(NewMissingTeams, _Points) -> NewMissingTeams.

%%%===================================================================
%%% Unit Test
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

team_points_to_map_test() ->
    ?assertEqual(
        {ok, #{<<"A">> => 1}, [<<"B">>, <<"C">>]},
        team_points_to_map([1], [<<"A">>, <<"B">>, <<"C">>])),
    ?assertEqual(
        {ok, #{<<"A">> => 1, <<"B">> => 2, <<"C">> => 3}, []},
        team_points_to_map([1, 2, 3], [<<"A">>, <<"B">>, <<"C">>])),
    ?assertEqual(
        {ok, #{<<"A">> => 1, <<"B">> => 2, <<"C">> => 3}, []},
        team_points_to_map([{2, <<"B">>}, {1, <<"A">>}, 3], [<<"A">>, <<"B">>, <<"C">>])),
    ?assertEqual({error, {not_recognized, <<"B">>}}, team_points_to_map([{2, <<"B">>}, {1, <<"A">>}, 3], [])),
    ?assertEqual({error, {not_recognized, <<"B">>}}, team_points_to_map([{2, <<"B">>}, {1, <<"A">>}, 3], [<<"A">>])),
    ?assertEqual({error, {multiple_matches, <<"A">>}}, team_points_to_map([{1, <<"A">>}], [<<"Alo">>, <<"Ama">>])),
    ?assertEqual(
        {ok, #{<<"Baba">> => 1}, [<<"A">>, <<"C">>]},
        team_points_to_map([{1, <<"B">>}], [<<"A">>, <<"Baba">>, <<"C">>])).

-endif.


maybe_show_results([], Msg, BotName, NewState) ->
    points(chat_id(Msg), BotName, NewState);
maybe_show_results(_, _, _, _) -> ok.
