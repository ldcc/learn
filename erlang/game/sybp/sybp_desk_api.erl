%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : desk_zpsybp_api.erl
%%% Created : 
%%% -------------------------------------------------------------------

-module(sybp_desk_api).

-behaviour(gamecore_api).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.game_zpsybp.hrl").
-include("mod.desk_scene.hrl").

%% --------------------------------------------------------------------
%% 以下系统默认导出(勿删)
-export([gc_mount/2,gc_switch/1,gc_callback_start/1,gc_callback_stop/0,msg/2,msg_group/2,data_get/1,data_set/2,cache_get/1,cache_set/2]).
%% 扩展导出
-export([
		 doloop/1,
		 record_desk/2,
		 record_ext_d/1,
		 record_seat/3,
		 desk_ready/3,
		 desk_join/2,
		 offline/2,
		 reconnect/2
		]).

-export([
		 load_cb/2,
		 desk_exit/3,
		 handle_event_cb/2,
		 player_alter_card_cb/2,
		 wechat_share_cb/2
		]).

-export([
		 test_event/2,
		 test_event_cb/2
		]).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

%% --------------------------------------------------------------------
%% @description: 模块安装
%% -------------------------------------------------------------------- 
gc_mount(IsRefresh,IsOpen)->
	EndMsgId = 0,
	ProcKey  = ?null,
	ExtsData = ?null,
	db:mount_add(EndMsgId,?MODULE,ProcKey,ExtsData,IsOpen,IsRefresh).

%% --------------------------------------------------------------------
%% @description: 禁用模块/开启模块
%% -------------------------------------------------------------------- 
gc_switch(Open) ->
	case Open of
		?true -> ?ok;
		?false-> ?ok
	end.

%% --------------------------------------------------------------------
%% @description: 开（节点）服  后 要调的写在这里
%% -------------------------------------------------------------------- 
gc_callback_start(_Cores)->
	?ok.

%% --------------------------------------------------------------------
%% @description: 关（节点）服 前 要调的写在这里
%% -------------------------------------------------------------------- 
gc_callback_stop()->
	?ok.

%% --------------------------------------------------------------------
%% @description: 数据转2进制
%% -------------------------------------------------------------------- 
% 对战结算 [4005]
msg(?A_DESK_ZPSYBP_VS_WAIT_END, {RoomKey, Timestamp, WinSeatIdx, {HuType, HuProfit}, IsDelay, CartTail, BankerIdx, Seats})->
	Bin1 = msg:encode([{?int32u, RoomKey}, {?int32u, Timestamp}, {?int8u,WinSeatIdx}, {?int8u,HuType}, {?int8,HuProfit}, {?bool,IsDelay}]),
	Bin2 = <<(msg_group(?A_DESK_ZPSYBP_DEAL, CartTail))/binary, (msg:encode([{?int8u, length(Seats)}]))/binary>>,
	Bin3 = iolist_to_binary([msg_group(?A_DESK_ZPSYBP_SEAT_SETTLE_INFO, {BankerIdx, Seat}) || Seat <- Seats]),
	msg:msg(?A_DESK_ZPSYBP_VS_WAIT_END, <<Bin1/binary, Bin2/binary, Bin3/binary>>);
	

% 庄家信息 [4099]
msg(?A_DESK_ZPSYBP_BANKER_INFO, {BankerIdx, BoutCurr, CardHeap, GameState})->
    Bin1 = msg_group(?A_DESK_ZPSYBP_BANKER_INFO, {BankerIdx, BoutCurr, CardHeap, GameState}),
    msg:msg(?A_DESK_ZPSYBP_BANKER_INFO, Bin1);


% 牌组 [4101]
msg(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, Behavior, Deals})->
	Bin1 = msg_group(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, Behavior, Deals}),
    msg:msg(?A_DESK_ZPSYBP_DEAL_GROUP, Bin1);


% 出牌/摸牌 [4102]
msg(?A_DESK_ZPSYBP_DISCARD, {SeatId, Behavior, Deal})->
	Bin1 = msg_group(?A_DESK_ZPSYBP_DISCARD, {SeatId, Behavior, Deal}),
    msg:msg(?A_DESK_ZPSYBP_DISCARD, Bin1);


% 可能触发的事件 [4103]
msg(?A_DESK_ZPSYBP_MAYBE_EVENT, Events)->
	Bin1 = msg_group(?A_DESK_ZPSYBP_MAYBE_EVENT, Events),
    msg:msg(?A_DESK_ZPSYBP_MAYBE_EVENT, Bin1);


% 对战重连块 [4310]
msg(?A_DESK_ZPSYBP_RECONNECT_INFO, {Desk, Seat})->
	#desk{state=DeskState,card_heap=CardHeap,curr_seat_idx=CurrIdx,seat_list=Seats,banker_idx=BankerIdx,event_wait=Des,bout_idx=BoutIdx,vote_list=Votes} = Desk,
	#zpsybp_d{focus_card=FocusCard,focus_method=Method} = Desk#desk.ext_d,
	#seat{uid=Uid,seat_id=SeatId,ext_s=#zpsybp_s{hand_deal=HandDeal}} = Seat,
	VoteInfo = lists:keyfind(?CONST_VIPM_VOTE_TYPE_DISSOLVE, #vote_info.vote_type, Votes),
	De = lists:keyfind(SeatId, #de.seat_idx, Des),
	Fun = fun(Idx, Behavior, Deal) -> msg_group(?A_DESK_ZPSYBP_DEAL_GROUP, {Idx, Behavior, Deal}) end,
	Comband = fun(Idx, Od, Bd) -> [Fun(Idx, ?eventing, lists:reverse(Od)), Fun(Idx, ?abandon, [Bd])] end,
	Bin1 = msg_group(?A_DESK_ZPSYBP_DISCARD, {CurrIdx, Method, [FocusCard]}),
	Bin2 = msg_group(?A_DESK_ZPSYBP_BANKER_INFO, {BankerIdx, BoutIdx, CardHeap, DeskState}),
	Bin3 = sybp_ai_mod:make_vote_msg(VoteInfo, Uid),
	Bin4 = sybp_ai_mod:make_maybe_msg(De, FocusCard, ?msg_group),
	Bin5 = Fun(SeatId, ?sortout, HandDeal),
	Bin6 = msg:encode([{?int8u, length(Seats)}]),
	Bin7 = iolist_to_binary([Comband(Idx, Od, Bd) || #seat{seat_id=Idx,ext_s=#zpsybp_s{open_deal=Od,ban_deal=Bd}} <- Seats]),
	msg:msg(?A_DESK_ZPSYBP_RECONNECT_INFO, <<Bin1/binary, Bin2/binary, Bin3/binary, Bin4/binary, Bin5/binary, Bin6/binary, Bin7/binary>>);


msg(MsgID,DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>.


%% --------------------------------------------------------------------
%% @description: 数据转2进制(协议块)
%% -------------------------------------------------------------------- 
% [协议块]玩家结算信息块 [4006]
msg_group(?A_DESK_ZPSYBP_SEAT_SETTLE_INFO, {BankerSeatId, Seat})->
	#seat{seat_id=SeatId,icon_id=HeadId,uname=Name,ext_s=#zpsybp_s{hu_acc=HuAcc,hu_curr=HuCurr,hand_deal=HandDeal,open_deal=OpenDeal}} = Seat,
	MyDeal = sybp_ai_api:append_by(fun(_, Deal) -> length(Deal) < ?three end, HandDeal, []) ++ OpenDeal,
	Bin1 = msg:encode([{?int8u, SeatId}, {?bool, BankerSeatId == SeatId}, {?int32u, HeadId}, {?string, Name}, {?int16, HuAcc}, {?int16, HuCurr}, {?int8u, length(MyDeal)}]),	
	Bin2 = iolist_to_binary([[msg:encode([{?int8u, sybp_ai_api:profit_get(Deal)}]), msg_group(?A_DESK_ZPSYBP_DEAL, Deal)] || Deal <- MyDeal]),
	<<Bin1/binary, Bin2/binary>>;


% 庄家信息 [4099]
msg_group(?A_DESK_ZPSYBP_BANKER_INFO, {BankerIdx, BoutCurr, CardHeap, DeskState})->
	IsGaming = DeskState == ?GAME_PREP orelse DeskState == ?GAME_OVER,
    msg:encode([{?int32u, BankerIdx}, {?int8u, BoutCurr}, {?int8u, length(CardHeap)}, {?bool, not IsGaming}]);


% [协议块]牌信息块 [4100]
msg_group(?A_DESK_ZPSYBP_DEAL, [Card|_]=Deal) when is_record(Card, bpc)->
	Bin1 = msg:encode([{?int8u, Card#bpc.state}, {?int8u, length(Deal)}]),
	Bin2 = iolist_to_binary([(msg:encode([{?int8u, K}, {?int8u, P}])) || #bpc{kind=K,point=P} <- Deal]),
	<<Bin1/binary, Bin2/binary>>;
msg_group(?A_DESK_ZPSYBP_DEAL, _Others)->
	State = ?CONST_ZPSYBP_CARD_STATE_NORMAL,
	msg:encode([{?int8u,State}, {?int8u,?zero}]);	


% [协议块]牌组 [4101]
msg_group(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, Behavior, Deals})->
	Bin1 = msg:encode([{?int8u, SeatId}, {?int8u, Behavior}, {?int8u, length(Deals)}]),
	Bin2 = iolist_to_binary([(msg_group(?A_DESK_ZPSYBP_DEAL, Deal)) || Deal <- Deals]),
    <<Bin1/binary, Bin2/binary>>;


% 出牌/摸牌 [4102]
msg_group(?A_DESK_ZPSYBP_DISCARD, {SeatId, Behavior, Deal})->
	Bin1 = msg:encode([{?int8u,SeatId}, {?int8u,Behavior}]),
	Bin2 = msg_group(?A_DESK_ZPSYBP_DEAL, Deal),
    <<Bin1/binary, Bin2/binary>>;


% [协议块]可能触发的事件 [4103]
msg_group(?A_DESK_ZPSYBP_MAYBE_EVENT, Events)->
	Fun = fun(Event, Deal) -> msg_group(?IF(Event == ?CONST_ZPSYBP_CARD_STATE_CHI, ?A_DESK_ZPSYBP_CHI_STRUCT, ?A_DESK_ZPSYBP_DEAL), Deal) end, 
	Bin1 = msg:encode([{?int8u, length(Events)}]), 
	Bin2 = iolist_to_binary([[msg:encode([{?int8u, Event}]), Fun(Event, Deal)] || {Event, Deal} <- Events]),
	<<Bin1/binary, Bin2/binary>>; 


% [协议块]吃牌(可吃结构) [4106]
msg_group(?A_DESK_ZPSYBP_CHI_STRUCT, [])->
	<<?zero>>;
msg_group(?A_DESK_ZPSYBP_CHI_STRUCT, DealData)->
	Bin1 = msg:encode([{?int8u, length(DealData)}]), 
	Bin2 = iolist_to_binary([[msg_group(?A_DESK_ZPSYBP_DEAL, Deal), msg_group(?A_DESK_ZPSYBP_CHI_STRUCT, Data)] || {Deal, Data} <- DealData]),
	<<Bin1/binary, Bin2/binary>>;


msg_group(MsgID,DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>. 


%% Data数据get
%%  @返回 Data
data_get(Gate) ->
	TagEtsMod = desk_zpsybp_ets,
	InitData  = ?init,
	app_tool:data_get(Gate, TagEtsMod, InitData).
%% Data数据set
%%  @返回 Gate
data_set(Gate,Data)->
	TagEtsMod = desk_zpsybp_ets,
	app_tool:data_set(Gate, TagEtsMod, Data).

%% Cache数据get
%%  @返回 Data
cache_get(Gate) ->
	TagEtsMod = desk_zpsybp_ets,
	InitData  = ?init,
	app_tool:cache_get(Gate,TagEtsMod,InitData).
%% Cache数据set
%%  @返回 Gate
cache_set(Gate,Data)->
	TagEtsMod = desk_zpsybp_ets,
	app_tool:cache_set(Gate, TagEtsMod,Data).


%% ====================================================================
%% Internal functions
%% ====================================================================

%%	游戏阶段定时检查 doloop
doloop(Desk) ->
	case Desk#desk.state of
		?GAME_START	-> 
			Now = util_time:seconds(),
			DeskNew = gameon(Desk#desk{time_start=Now});
		?GAME_OVER	-> 
			Now = util_time:seconds(),
			DeskNew = gameover(Desk#desk{time_end=Now});
		?FLASHCARD	-> 
			DeskNew = flashcard(Desk); 
		?DRAWING	-> 
			DeskNew = drawing(Desk);
		?RECEIVING	-> 
			DeskNew = receiving(Desk);
		_DeskState		-> 
			DeskNew = Desk
	end,
	#desk{delay=Delays} = DeskNew,
	NewDelays = sybp_ai_mod:handle_delay_action(Delays),
	{?noreply, DeskNew#desk{delay=NewDelays}}.


%%	初始化桌子
record_desk(Desk,Vipm)->
	ExtD  = record_ext_d(Vipm),
	ExtDm = ?IF(is_record(Vipm, vipm), record_ext_dm(Vipm), ?EMPTY_RECORD),
	Desk#desk{ext_d=ExtD,ext_dm=ExtDm}.

%%	桌子基本数据
record_ext_d(Vipm)->
	#vipm{args={_SatisKeepBanker, _HuRule, PlayerLimit}} = Vipm,
	#zpsybp_d{player_limit=PlayerLimit}.

%%	好友房桌子数据
record_ext_dm(Vipm)->
	#vipm{args={SatisKeepBanker, HuRule, _PlayerLimit}} = Vipm,
	#zpsybp_dm{hu_rule=HuRule,satis_keep_banker=SatisKeepBanker}.


%%	初始化位置
record_seat(Seat, IsVipm, Args)->
	ExtS  = record_seat_ext_s(Args),
	ExtSM = ?IF(IsVipm == ?true, record_seat_ext_sm(Args) , ?EMPTY_RECORD),
	Seat#seat{ext_s=ExtS,ext_sm=ExtSM}.

%%	位置基本数据
record_seat_ext_s (_Args)->
	#zpsybp_s{}.

%%	好友房位置数据
record_seat_ext_sm(_Args)->
	#zpsybp_sm{}.


%% ====================================================================
%% ====================================================================
%% ====================================================================
%% 加入房间
desk_join(Desk, Seat)->
	#desk{game_id=GameId,room_id=RoomId,desk_id=DeskId} = Desk,
	#seat{uid=Uid} = Seat,
	desk_api:join_user_ok(Uid, self(), RoomId, DeskId, GameId),
	{?noreply,Desk}.

%% 退出
offline(Desk, _Uid)->
	{?noreply, Desk}.

%% 重连对战
reconnect(Desk, Gate) ->
	#desk{game_id=GameId,seat_list=SeatsOld,event_wait=Des,ext_d=ExtD} = Desk,
	#zpsybp_d{player_limit=Limit} = ExtD,
	SeatTmp = vipm_api:vipm_seat_record(Gate, GameId, ?null),
	SeatsNew = [#seat{seat_id=SeatId}|_Tail] = sybp_ai_mod:seats_renew(SeatsOld, SeatTmp, Limit),
	case lists:keytake(SeatId, #de.seat_idx, Des) of
		?false				-> 
			{?noreply, Desk#desk{seat_list=SeatsNew}};
		{?value, De, Tail}	-> 
			DesNew = [De#de{mpid=Gate#gate.mpid} | Tail],
			{?noreply, Desk#desk{seat_list=SeatsNew,event_wait=DesNew}}
	end.

%% 请求对战(准备好了)
desk_ready(Desk, _Uid, _IsReady)->
	#desk{seat_list=Seats,ext_d=ExtD} = Desk,
	#zpsybp_d{player_limit=PlayerLimit} = ExtD,
	case length([IsReady || #seat{is_ready=IsReady} <- Seats, IsReady]) == PlayerLimit of
		?false	-> 
			{?noreply, Desk};
		?true	-> 
			{?noreply, Desk#desk{state=?GAME_START}}
	end.

%% 起身/换坐/退出
desk_exit(Desk, Uid, {Type, _SeatId}) ->
	#desk{master_uid=MasterUid,vipm_key=VipmKey,seat_list=Seats,game_id=GameId,desk_id=DeskId,desk_num=Num,bout_idx=BoutIdx} = Desk,
	#seat{uid=Uid,mpid=Mpid} = Seat = lists:keyfind(Uid, #seat.uid, Seats),
	case BoutIdx > ?zero of 
		?true	->
			Num2 = Num,
			Seats2 = Seats,
			system_api:send_error(Mpid, ?E_PRIVATE_ROOM_CANT_OUT);
		?false	->
			case Type of
				?seat_up	->
					Num2 = Num,
					Seat2 = Seat#seat{seat_id=?seat_up},
					Seats2 = lists:keyreplace(Uid, #seat.uid, Seats, Seat2),
					BinMsg = desk_api:msg(?A_DESK_EXIT, {GameId, DeskId, ?CONST_VS_STATE_LEAVE, Uid}),
					msg:send(Mpid, BinMsg);
				?seat_out	->
					Num2 = Num - 1,
					Seats2 = lists:delete(Seat, Seats),
					desk_api:user_clean(Uid),
					dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_cut, [VipmKey, Uid]),
					BinMsg = desk_api:msg(?A_DESK_EXIT, {GameId, DeskId, ?CONST_VS_STATE_LEAVE, Uid}),
					desk_api:broadcast(Desk, BinMsg),
					vipm_api:notify_reflesh(MasterUid);
				?seat_alt	->
					Num2 = Num,
					Seats2 = Seats
			end
	end,
	{?noreply, Desk#desk{seat_list=Seats2,desk_num=Num2}}.


%% ====================================================================
%% ====================================================================
%% ====================================================================
%% ====================================================================

%%	前端切换 场景成功回调
load_cb(Desk, {Uid, Mpid, IsReconn}) ->
	#desk{room_type=RoomType,seat_list=Seats} = Desk,
	Seat = lists:keyfind(Uid, #seat.uid, Seats),
	case RoomType of
		?CONST_SCENE_TYPE_GOLD	-> 
			?skip;
		?CONST_SCENE_TYPE_KA	->
			[msg:send(Mpid, sybp_ai_mod:make_seat_msg(Desk, Seat)) || Seat <- Seats],
			msg:send(Mpid, sybp_ai_mod:make_desk_msg(Desk));
		?CONST_SCENE_TYPE_GROUP	-> 
			?skip
	end,
	?IF(IsReconn, msg:send(Mpid, msg(?A_DESK_ZPSYBP_RECONNECT_INFO, {Desk, Seat})), ?skip),
	{?noreply, Desk}.


%%	触发事件回调
handle_event_cb(Desk, {Uid, State, Deal}) ->
	#desk{state=DeskState,seat_list=Seats} = Desk,
	case DeskState == ?GAME_PREP orelse DeskState == ?GAME_OVER of
		?true	-> 
			{?noreply, Desk};
		?false	->
			DeskNew = sybp_desk_mod:handle_event(Desk, lists:keyfind(Uid, #seat.uid, Seats), {State, Deal}),
			{?noreply, DeskNew#desk{time_last=util_time:seconds()}}
	end.


%%	玩家换牌回调
player_alter_card_cb(Desk, {Uid, Vectors, Cards}) ->
	#desk{seat_list=Seats} = Desk,
	case lists:keytake(Uid, #seat.uid, Seats) of
		{?value, Seat, Tail}	->
			#seat{seat_id=SeatId,mpid=Mpid,ext_s=#zpsybp_s{hand_deal=HandDeal}=ExtS} = Seat,
			case lists:sort([Deal || Deal <- lists:flatten(HandDeal), Deal =/= []]) == lists:sort(Cards) of
				?true	->
					VectorDeals = [lists:reverse(Deal) || Deal <- sybp_ai_api:make_vector(lists:zip(Vectors, Cards), [])],
					{?noreply, Desk#desk{seat_list=[Seat#seat{ext_s=ExtS#zpsybp_s{hand_deal=VectorDeals}}|Tail]}};
				?false	->
					begin msg:send(Mpid, sybp_desk_api:msg(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, ?sortout, HandDeal})), {?noreply, Desk} end
			end;
		?false	->
			{?noreply, Desk}
	end.	


%%	微信分享回调
wechat_share_cb(Desk, Mpid) ->
	#desk{ext_d=#zpsybp_d{player_limit=Limit},ext_dm=#zpsybp_dm{satis_keep_banker=IsKeep,hu_rule=Rule}} = Desk,
	Names = [Name || #seat{uname=Name} <- Desk#desk.seat_list],
	msg:send(Mpid, sybp_vipm_api:msg(?A_VIPM_ZPSYBP_WECHAT_SHARE_ACK, {IsKeep, Rule, Limit, Names})),
	{?noreply, Desk}.


%%	测试触发事件
test_event(Uid, Bin) ->
	msg:send_pid(Uid, ?MODULE, test_event_cb, Bin).

test_event_cb(Gate, Bin) ->
	sybp_desk_gate:way(?R_DESK_ZPSYBP_EVENT, Gate, Bin).


%% ====================================================================
%% ====================================================================
%% ====================================================================
%% ====================================================================

%%	开始游戏
gameon(Desk) ->
	#desk{master_uid=MasterUid,bout_idx=BoutIdx} = Desk,
	DeskNew = sybp_desk_mod:game_init(Desk),
	vipm_api:notify_reflesh(MasterUid),
	DeskNew#desk{is_start_game=?true,is_cancle_dismiss=?true,bout_idx=BoutIdx+?one,state=?FLASHCARD,state_tag=?null,state_time=?zero}.


%%	亮张
%% flashcard(Desk) ->
flashcard(Desk) ->
	#desk{state_time=StateTime} = Desk,
	case StateTime of
		?zero		->
			DeskNew = sybp_desk_mod:flashcard_begin(Desk),
			DeskNew#desk{state_time=StateTime+1};
		StateTime		-> 
			DeskNew = sybp_desk_mod:flashcard_over(Desk),
			DeskNew#desk{state_time=?zero}
	end.


%%	等待接收事件ing
receiving(Desk) when Desk#desk.ext_d#zpsybp_d.focus_card == [] ->
	Desk#desk{state=?DRAWING,state_time=?zero};
receiving(Desk) when Desk#desk.event_wait == [], Desk#desk.event_ing == ?null ->
	#desk{seat_list=Seats,curr_seat_idx=CurrIdx,ext_d=#zpsybp_d{focus_card=FocusCard}} = Desk,
	{?value, #seat{ext_s=#zpsybp_s{ban_deal=BanDeal}=ExtS}=Seat, Tail} = lists:keytake(CurrIdx, #seat.seat_id, Seats),
	Desk#desk{state=?DRAWING,state_time=?zero,seat_list=[Seat#seat{ext_s=ExtS#zpsybp_s{ban_deal=[FocusCard|BanDeal]}}|Tail]};
receiving(Desk) ->
	sybp_desk_mod:receiving(Desk).


%%	摸牌
drawing(Desk) ->
	#desk{state_time=StateTime} = Desk,
	case StateTime of
		?zero		-> 
			(sybp_desk_mod:drawing(Desk))#desk{state_time=StateTime+1};
		StateTime	-> 
			Desk#desk{state=?RECEIVING,state_time=?zero}
	end.


%%	游戏结束
gameover(Desk) ->
	sybp_ai_mod:send_vslogs_msg(Desk),
	sybp_desk_mod:game_over(Desk).
