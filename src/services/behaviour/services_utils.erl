%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. May 2022 15:10
%%%-------------------------------------------------------------------
-module(services_utils).
-author("stefano.bertolotto").

%% API
-export([parse_num_words/1]).

- define(IS_NUM(N),
    (
        N =:= $0 orelse
        N =:= $1 orelse
            N =:= $2 orelse
            N =:= $3 orelse
            N =:= $4 orelse
            N =:= $5 orelse
            N =:= $6 orelse
            N =:= $7 orelse
            N =:= $8 orelse
            N =:= $9
    )
).

parse_num_words(BinText) ->
    Text = binary_to_list(BinText),
    ListData = parse_num_words(Text, {number, [], []}),
    lists:map(
        fun
            ({N, T}) -> {list_to_integer(N), binary:list_to_bin(clean(T))};
            (N) -> list_to_integer(N)
        end,
        ListData
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_num_words([], {number, N, Acc}) when N =/= [] ->
    lists:reverse([N | Acc]);
parse_num_words([], {name, Name, [LastNum | Acc]} = L) when Name =/= [] ->
    lists:reverse([{LastNum, Name} | Acc]);
parse_num_words([], {_, _, Acc}) ->
    lists:reverse(Acc);
parse_num_words([Num | Rest], {number, CurrNum, Acc}) when ?IS_NUM(Num) ->
    parse_num_words(Rest, {number, CurrNum ++ [Num], Acc});
parse_num_words([$  | Rest], {number, [], Acc}) ->
    parse_num_words(Rest, {number_end, [], Acc});
parse_num_words([$  | Rest], {number, CurrNum, Acc}) ->
    parse_num_words(Rest, {number_end, [], [CurrNum] ++ Acc});
parse_num_words([Name | Rest], {number, Number, Acc}) ->
    parse_num_words(Rest, {name, [Name], [Number] ++ Acc});

parse_num_words([Num | Rest], {number_end, [], Acc}) when ?IS_NUM(Num) ->
    parse_num_words(Rest, {number, [Num], Acc});
parse_num_words([$  | Rest], {number_end, [], Acc}) ->
    parse_num_words(Rest, {number_end, [], Acc});
parse_num_words([Name | Rest], {number_end, [], Acc}) ->
    parse_num_words(Rest, {name, [Name], Acc});

parse_num_words([Num | Rest], {name, CurrName, [LastNum | Acc]}) when ?IS_NUM(Num) ->
    parse_num_words(Rest, {number, [Num], [{LastNum, CurrName} | Acc]});
parse_num_words([Name | Rest], {name, CurrName, Acc}) ->
    parse_num_words(Rest, {name, CurrName ++ [Name], Acc}).

clean(Text) ->
    lists:reverse(trim_spaces(lists:reverse(Text))).

trim_spaces([$  | Text]) -> trim_spaces(Text);
trim_spaces(Text)        -> Text.

%%%===================================================================
%%% Unit Test
%%%===================================================================

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

get_data_test() ->
    ?assertEqual([], parse_num_words(<<"">>)),
    ?assertEqual([1], parse_num_words(<<"1">>)),
    ?assertEqual([1, 2, 3], parse_num_words(<<"1 2 3">>)),
    ?assertEqual(
        [{1, <<"Alfred">>}, {2, <<"Ginny">>}, {3, <<"Itch">>}],
        parse_num_words(<<"1Alfred 2 Ginny 3Itch">>)),
    ?assertEqual([1, {2, <<"Cool">>}, {4, <<"School">>}], parse_num_words(<<"1 2Cool 4 School">>)),
    ?assertEqual([{1, <<"A b">>}, {2, <<"c">>}], parse_num_words(<<"1 A b 2 c">>)),
    ?assertEqual([23, {12, <<"A">>}], parse_num_words(<<"23 12 A">>)).

-endif.
