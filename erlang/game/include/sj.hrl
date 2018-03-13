%%%-------------------------------------------------------------------
%%% @author ldcc
%%% @copyright (C) 2018, <ldcc>
%%% Created : 28. 二月 2018 11:38
%%%-------------------------------------------------------------------
-author("ldcc").

-define(DEAL_NORMAL, 0).
-define(DEAL_SIGLE, 1).
-define(DEAL_COUPLE, 2).
-define(DEAL_TRACTOR, 3).
-define(DEAL_THROW, 4).
-define(SJ_NUM_PLAYER, 4).
-define(SJ_NUM_CARD, 25).
-define(SJ_DEAL_TIME, 30).


-record(sj_d, {
    winner = 0 :: byte(),
    profit = 0 :: non_neg_integer(),
    trump_suit = 0 :: byte(),
    trump_point = 0 :: byte(),
    lord_data = {0, []} :: tuple()
}).

-record(sj_dm, {
    game_mode = 0 :: byte(),
    multiple = 0 :: byte(),
    default_level = 0 :: byte(),
    is_uplevel = false :: boolean()
}).

-record(sj_s, {
    tractor_times = 0 :: byte(),
    defeat_times = 0 :: byte(),
    lord_times = 0 :: byte(),
    win_times = 0 :: byte(),
    level = 0 :: byte(),
    score = 0 :: integer(),
    offset_score = 0 :: integer(),
    offset_level = 0 :: byte(),
    stand_for = 0 :: byte(),
    hand_cards = [] :: [tuple()]
}).

-record(sjc, {
    suit = 0 :: byte(),
    point = 0 :: byte(),
    ref_point = 0 :: byte()
}).

-define(attacker, 1).
-define(defender, 2).
-define(diamonds, 1).
-define(clubs, 2).
-define(hearts, 3).
-define(spades, 4).
-define(trump, 5).
-define(ace, 14).
-define(extend_point, 30).
-define(suits, lists:seq(1, 4)).
-define(points, lists:seq(2, 14)).
-define(joker_point, lists:seq(16, 17)).

-define(bout, 1).
-define(level, 2).
-define(dealing, 1).
-define(lording, 2).
-define(burying, 3).
-define(leading, 4).
-define(fliping, 5).
-define(alt_pointer, 6).
-define(prompt, 7).

-define(justlord, 11).
-define(defended, 12).
-define(selfruin, 13).
-define(rushlord, 14).

-define(justlead, 1).
-define(defeat, 2).
-define(defeatist, 3).

-define(GAME_PREP, 0).
-define(GAME_ON, 1).
-define(GAME_PAUSE, 2).
-define(GAME_OVER, 3).
-define(GAME_DISBAND, 4).

-define(GAME_LORD, 10).
-define(GAME_LEAD, 11).                                                           
