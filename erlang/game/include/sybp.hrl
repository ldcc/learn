%%%-------------------------------------------------------------------
%%% @author ldcc
%%% @copyright (C) 2018, <ldcc>
%%% Created : 28. 二月 2018 11:56
%%%-------------------------------------------------------------------
-author("ldcc").

-define(DEAL_NORMAL, 0).
-define(DEAL_CHOU, 1).
-define(DEAL_CHI, 2).
-define(DEAL_PONG, 3).
-define(DEAL_PAO, 4).
-define(DEAL_PPAO, 5).
-define(DEAL_WPAO, 6).
-define(DEAL_WIN, 7).
-define(DEAL_WEI, 8).
-define(DEAL_CHOWEI, 9).
-define(DEAL_TI, 10).
-define(DEAL_WTI, 11).
-define(DEAL_KAN, 100).

-record(sybp_d, {
    player_limit = 0 :: byte(),
    focus_idx = 0 :: byte(),
    focus_card = null :: bpc(),
    focus_method = 0 :: byte(),
    hu_data = null :: [bpc()]
}).

-record(sybp_dm, {
    hu_rule = 0 :: byte(),
    last_banker_profit = 0 :: non_neg_integer(),
    satis_keep_banker = false :: boolean()
}).

-record(sybp_s, {
    hu_acc = 0 :: byte(),
    hu_curr = 0 :: byte(),
    hu_times = 0 :: byte(),
    sd_times = 0 :: byte(),
    ti_times = 0 :: byte(),
    pao_times = 0 :: byte(),
    ban_deal = [] :: [bpc()],
    pass_deal = [] :: [bpc()],
    hand_deal = [] :: [bpc()],
    open_deal = [] :: [bpc()]
}).

-type bpc() :: {bpc, byte(), byte(), byte()}.

-record(bpc, {
    kind = 0 :: byte(),
    state = 0 :: byte(),
    point = 0 :: byte()
}).

-define(dealing, 1).
-define(sortout, 2).
-define(eventing, 3).
-define(alt_pointer, 4).
-define(flashcard, 1).
-define(discard, 2).
-define(drawcard, 3).
-define(abandon, 4).

-define(natural, {1, 10}).
-define(unnatural, {2, 10}).
-define(selfdraw, {3, 10}).
-define(justhu, {4, 0}).
-define(barren, {5, -10}).

-define(TYPE_UPPER, 1).
-define(TYPE_LOWER, 2).
-define(COLOR_RED, 1).
-define(COLOR_BLACK, 2).
-define(PLAYER_NUM1, 3).
-define(PLAYER_NUM2, 4).
-define(CARD_NUM1, 20).
-define(CARD_NUM2, 14).
-define(COLUMN, 10).
-define(ROW, 3).

-define(CARD_TYPE, [1, 2]).
-define(CARD_POINT, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]).

-define(GAME_PREP, 0).
-define(GAME_START, 1).
-define(GAME_PAUSE, 2).
-define(GAME_OVER, 3).
-define(DISBAND_ROOM, 4).

-define(FLASHCARD, 10).
-define(DRAWING, 11).
-define(DISCARDING, 12).
-define(RECEIVING, 15).

-define(WEIS, [?DEAL_WEI]).
-define(TIS, [?DEAL_TI, ?DEAL_WTI]).
-define(PAOS, [?DEAL_PAO, ?DEAL_WPAO, ?DEAL_PPAO]).
-define(PASS, [?DEAL_CHI, ?DEAL_PONG]).

-define(AUTO_EVENTS, ?WEIS ++ ?TIS ++ ?PAOS).

-define(DISCARD_NEXT_EVENTS, [?DEAL_PONG, ?DEAL_PAO, ?DEAL_WPAO, ?DEAL_CHI]).

-define(DISCARD_OTHER_EVENTS, [?DEAL_PONG, ?DEAL_PAO, ?DEAL_WPAO]).

-define(DRAW_SELF_EVENTS, [?DEAL_WIN, ?DEAL_PPAO, ?DEAL_CHI | ?TIS ++ ?WEIS]).

-define(DRAW_NEXT_EVENTS, [?DEAL_WIN, ?DEAL_PONG, ?DEAL_CHI | ?PAOS]).

-define(DRAW_OTHER_EVENTS, [?DEAL_WIN, ?DEAL_PONG | ?PAOS]).

-define(make123(Kind), [#bpc{kind = Kind, point = Point} || Point <- [1, 2, 3]]).
-define(make274(Kind), [#bpc{kind = Kind, point = Point} || Point <- [2, 7, 10]]).
