%%%-------------------------------------------------------------------
%%% @author stefano.bertolotto
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2022 11:25
%%%-------------------------------------------------------------------
-author("stefano.bertolotto").

-define(CALLBACK_ARGS, Msg :: map(), Name :: bot_name(), ServiceState :: map()).
-define(CALLBACK_RES, ok | {ok, NewState :: map()}).