%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : desk_sj_api.erl
%%% Created : 
%%% -------------------------------------------------------------------
-module(desk_api).

-behaviour(gamecore_api).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.game_sj.hrl").
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
		 eventing_cb/2,
		 wechat_share_cb/2
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
	SjsData = ?null,
	db:mount_add(EndMsgId,?MODULE,ProcKey,SjsData,IsOpen,IsRefresh).

%% --------------------------------------------------------------------
%% @description: 禁用模块/开启模块
%% -------------------------------------------------------------------- 
gc_switch(Open)->
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
msg(?A_DESK_SJ_SETTLEMENT, {Profit, Winner, LevelUp, BankerUid, Seats})->
    Bin1 = msg:encode([{?int32u, Profit}, {?int8u, Winner}, {?int8u, LevelUp}, {?int32u, BankerUid}, {?int8u, length(Seats)}]),
	Bin2 = iolist_to_binary([msg_group(?A_DESK_SJ_SETTLE_INFO, Seat) || Seat <- Seats]),
	msg:msg(?A_DESK_SJ_SETTLEMENT, <<Bin1/?binary, Bin2/?binary>>);

% 打牌数据 [4098]
msg(?A_DESK_SJ_SCORE_DATA, {ProfitAcc, ProfitCurr})->
	Bin1 = msg_group(?A_DESK_SJ_SCORE_DATA, {ProfitAcc, ProfitCurr}),
    msg:msg(?A_DESK_SJ_SCORE_DATA, Bin1);

% 游戏数据 [4099]
msg(?A_DESK_SJ_GAME_DATA, {GameState, BoutCurr, BoutAcc, Buid, Blevel, Seats})->
	Bin1 = msg_group(?A_DESK_SJ_GAME_DATA, {GameState, BoutCurr, BoutAcc, Buid, Blevel, Seats}),
    msg:msg(?A_DESK_SJ_GAME_DATA, Bin1);

% 触发牌事件 [4101]
msg(?A_DESK_SJ_CARDS_EVENT, {Uid, Behavior, Cards})->
    Bin1 = msg_group(?A_DESK_SJ_CARDS_EVENT, {Uid, Behavior, Cards}),
    msg:msg(?A_DESK_SJ_CARDS_EVENT, Bin1);

% 出牌 [4104]
msg(?A_DESK_SJ_LEADING, {Uid, IsSucc, IsFirst, Type, Action, BiggestUid, Cards})->
	Bin1 = msg_group(?A_DESK_SJ_LEADING, {Uid, IsSucc, IsFirst, Type, Action, BiggestUid, Cards}),
    msg:msg(?A_DESK_SJ_LEADING, Bin1);

% 抄底 [4105]
msg(?A_DESK_SJ_FLIPING, {Flipper, Cards, {ProfitAcc, ProfitCurr}})->
    Bin1 = msg:encode([{?bool, Flipper == ?attacker}]),
	Bin2 = msg_group(?A_DESK_SJ_CARDS_EVENT, {?zero, ?fliping, Cards}),
	Bin3 = msg_group(?A_DESK_SJ_SCORE_DATA, {ProfitAcc, ProfitCurr}),
    msg:msg(?A_DESK_SJ_FLIPING, <<Bin1/?binary, Bin2/?binary, Bin3/?binary>>);

% 对战重连块 [4310]
msg(?A_DESK_SJ_RECONNECT_INFO, {Desk, Seat})->
	#desk{state=GameState,card_heap=Flip,event_ing=Eing,seat_list=Seats,banker_idx=Buid,curr_uid=Cuid,bout_idx=Bidx,bout_total=Btotal,vote_list=Votes} = Desk,
	#sj_d{lord_data={Luid,Lcards},profit=Profit,trump_suit=Ts,trump_point=Tp} = Desk#desk.ext_d,
	#seat{uid=Uid,ext_s=#sj_s{hand_cards=Hcs}} = Seat,
	VoteInfo = lists:keyfind(?CONST_VIPM_VOTE_TYPE_DISSOLVE, #vote_info.vote_type, Votes),
	MsgVote = ai_mod:make_vote_msg(VoteInfo, Uid),
	MsgScore = msg_group(?A_DESK_SJ_SCORE_DATA, {Profit, ?zero}),
	MsgGame = msg_group(?A_DESK_SJ_GAME_DATA, {GameState, Bidx, Btotal, Buid, Tp, Seats}),
	MsgPrompt = msg_group(?A_DESK_SJ_CARDS_EVENT, {Cuid, ?alt_pointer, []}),
	MsgHcs = msg_group(?A_DESK_SJ_CARDS_EVENT, {Uid, ?dealing, Hcs}),
	MsgLord = msg_group(?A_DESK_SJ_CARDS_EVENT, {Luid, ?justlord, Lcards}),
	MsgBury = <<(msg:encode([{?bool, Flip == []}]))/?binary, (msg_group(?A_DESK_SJ_CARDS_EVENT, {Buid, ?burying, ?IF(Uid == Buid, Flip, [])}))/?binary>>,
	MsgOpen = ai_mod:make_open_msg(Ts, Tp, Eing),
	BinAcc = <<MsgVote/?binary, MsgScore/?binary, MsgGame/?binary, MsgPrompt/?binary, MsgHcs/?binary, MsgLord/?binary, MsgBury/?binary, MsgOpen/?binary>>,
	msg:msg(?A_DESK_SJ_RECONNECT_INFO, BinAcc);

msg(MsgID,DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>.


%% --------------------------------------------------------------------
%% @description: 数据转2进制(协议块)
%% --------------------------------------------------------------------
% [协议块]玩家结算信息块 [4006] 
msg_group(?A_DESK_SJ_SETTLE_INFO, Seat)->
	#seat{uid=Uid,uname=Name,ext_s=#sj_s{level=Level,stand_for=Stf,offset_score=Offset}} = Seat,
    msg:encode([{?int32u,Uid}, {?string, Name}, {?int8u, Level}, {?int8u, Stf}, {?int32, Offset}]);

% [协议快]分数数据 [4098]
msg_group(?A_DESK_SJ_SCORE_DATA, {ProfitAcc, ProfitCurr})->
    msg:encode([{?int32u, ProfitAcc}, {?int32u, ProfitCurr}]);

% [协议块]游戏数据 [4099]
msg_group(?A_DESK_SJ_GAME_DATA, {GameState, BoutCurr, BoutAcc, Buid, Blevel, Seats})->
	IsGaming = GameState == ?GAME_PREP orelse GameState == ?GAME_OVER,
    Bin1 = msg:encode([{?bool, IsGaming}, {?int8u, BoutCurr}, {?int8u, BoutAcc}, {?int32u, Buid}, {?int8u, Blevel}, {?int8u, length(Seats)}]),
	Bin2 = iolist_to_binary([msg:encode([{?int32u, Uid}, {?int8u, SjS#sj_s.level}, {?int8u, SjS#sj_s.stand_for}, {?int32, SjS#sj_s.score}]) || #seat{uid=Uid,ext_s=SjS} <- Seats]),
    <<Bin1/?binary, Bin2/?binary>>;

% [协议块]触发牌事件 [4101]
msg_group(?A_DESK_SJ_CARDS_EVENT, {Uid, Behavior, Cards})->
    Bin1 = msg:encode([{?int32u, Uid}, {?int8u, Behavior}, {?int8u, length(Cards)}]),
	Bin2 = <<(msg:encode([{?int8u, S}, {?int8u, P}])) || #sjc{suit=S,point=P} <- Cards>>,
	<<Bin1/?binary, Bin2/?binary>>;

% [协议块]出牌 [4104]
msg_group(?A_DESK_SJ_LEADING, {Uid, IsSucc, IsFirst, Type, Action, BiggestUid, Cards})->
    Bin1 = msg:encode([{?bool, IsSucc}, {?bool, IsFirst}, {?int8u, Type}, {?int8u, Action}, {?int32u, BiggestUid}]),
	Bin2 = msg_group(?A_DESK_SJ_CARDS_EVENT, {Uid, ?leading, Cards}),
	<<Bin1/?binary, Bin2/?binary>>;

msg_group(MsgID, DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>. 

%% Data数据get
%%  @返回 Data
data_get(Gate) ->
	TagEtsMod = desk_sj_ets,
	InitData  = ?init,
	app_tool:data_get(Gate, TagEtsMod, InitData).
%% Data数据set
%%  @返回 Gate
data_set(Gate,Data)->
	TagEtsMod = desk_sj_ets,
	app_tool:data_set(Gate, TagEtsMod, Data).

%% Cache数据get
%%  @返回 Data
cache_get(Gate) ->
	TagEtsMod = desk_sj_ets,
	InitData  = ?init,
	app_tool:cache_get(Gate,TagEtsMod,InitData).
%% Cache数据set
%%  @返回 Gate
cache_set(Gate,Data)->
	TagEtsMod = desk_sj_ets,
	app_tool:cache_set(Gate, TagEtsMod,Data).


%% ====================================================================
%% Internal functions
%% ====================================================================

%%	游戏阶段定时检查 doloop
doloop(Desk) ->
	case Desk#desk.state of
		?GAME_ON	-> 
			Now = util_time:seconds(),
			DeskNew = game_on(Desk#desk{time_start=Now});
		?GAME_OVER	-> 
			Now = util_time:seconds(),
			DeskNew = game_over(Desk#desk{time_end=Now});
		?GAME_LORD	->
			DeskNew = lording(Desk);
		?GAME_LEAD	->
			DeskNew = leading(Desk);
		_DeskState	-> 
			DeskNew = Desk
	end,
	#desk{delay=Delays} = DeskNew,
	NewDelays = util:handle_delay_action(Delays),
	{?noreply, DeskNew#desk{delay=NewDelays}}.


%%	初始化桌子
record_desk(Desk,Vipm)->
	SjD  = record_ext_d(Vipm),
	SjDm = ?IF(is_record(Vipm, vipm), record_ext_dm(Vipm), ?EMPTY_RECORD),
	Desk#desk{ext_d=SjD,ext_dm=SjDm}.

%%	桌子基本数据
record_ext_d(Vipm)->
	#vipm{args={_PlayMode, Level, _IsLevelUp, _Multiple}} = Vipm,
	#sj_d{trump_point=Level}.

%%	好友房桌子数据
record_ext_dm(Vipm)->
	#vipm{args={PlayMode, Level, IsLevelUp, Multiple}} = Vipm,
	#sj_dm{game_mode=PlayMode,multiple=Multiple,default_level=Level,is_uplevel=IsLevelUp}.

%%	初始化座位
record_seat(Seat, IsVipm, Args)->
	SjS  = record_seat_ext_s(Args),
	SjSm = ?IF(IsVipm == ?true, record_seat_ext_sm(Args) , ?EMPTY_RECORD),
	Seat#seat{ext_s=SjS,ext_sm=SjSm}.

%%	位置基本数据
record_seat_ext_s (_Args)->
	#sj_s{}.

%%	好友房位置数据
record_seat_ext_sm(_Args)->
	#sj_sm{}.


%% ====================================================================
%% ====================================================================
%% ====================================================================
%% 加入房间
desk_join(Desk, Seat)->
	#desk{game_id=GameId,room_id=RoomId,desk_id=DeskId} = Desk,
	#seat{uid=Uid} = Seat,
	desk_api:join_user_ok(Uid, self(), RoomId, DeskId, GameId),
	{?noreply, Desk}.

%% 退出
offline(Desk, _Uid)->
	{?noreply, Desk}.

%% 重连对战
reconnect(Desk, Gate) ->
	#desk{game_id=GameId,seat_list=SeatsOld,event_wait=Des} = Desk,
	SeatTmp = vipm_api:vipm_seat_record(Gate, GameId, ?null),
	SeatsNew = [#seat{seat_id=SeatId}|_Tail] = ai_mod:seats_renew(SeatsOld, SeatTmp),
	case lists:keytake(SeatId, #de.seat_idx, Des) of
		?false				-> 
			{?noreply, Desk#desk{seat_list=SeatsNew}};
		{?value, De, Tail}	-> 
			DesNew = [De#de{mpid=Gate#gate.mpid} | Tail],
			{?noreply, Desk#desk{seat_list=SeatsNew,event_wait=DesNew}}
	end.

%% 请求对战(准备好了)
desk_ready(Desk, _Uid, _IsReady)->
	#desk{seat_list=Seats} = Desk,
	case lists:all(fun(Seat) -> Seat#seat.is_ready end, Seats) of
%% 	case length([IsReady || #seat{is_ready=IsReady} <- Seats, IsReady]) == ?CONST_SJ_GAME_STATE_NUM_PLAYER of
		?false	-> 
			{?noreply, Desk};
		?true	-> 
			{?noreply, Desk#desk{state=?GAME_ON}}
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
			[msg:send(Mpid, ai_mod:make_seat_msg(Desk, Seat)) || Seat <- Seats],
			msg:send(Mpid, ai_mod:make_desk_msg(Desk));
		?CONST_SCENE_TYPE_GROUP	-> 
			?skip
	end,
	?IF(IsReconn, msg:send(Mpid, msg(?A_DESK_SJ_RECONNECT_INFO, {Desk, Seat})), ?skip),
	{?noreply, Desk}.


%%	触发事件回调
eventing_cb(Desk, {Behavior, Uid, Mpid, Cards}) ->
	#desk{seat_list=Seats,state=DeskState,room_type=RoomType} = Desk,
	SendError = fun(ErrorCode) -> begin system_api:send_error(Mpid, ErrorCode), Desk end end,
	case DeskState == ?GAME_PREP orelse DeskState == ?GAME_OVER of
		?true	-> 
			{?noreply, Desk};
		?false	->
			case RoomType of
				?CONST_SCENE_TYPE_GOLD	->
					?skip;
				?CONST_SCENE_TYPE_KA	->
					?skip;
				?CONST_SCENE_TYPE_GROUP	->
					?skip
			end,
			case lists:keyfind(Uid, #seat.uid, Seats) of
				?false	->
					{?noreply, SendError(?E_UNKNOWN)};
				Seat	->
					#seat{ext_s=#sj_s{hand_cards=Hcs}} = Seat,
					case length(Hcs -- Cards) == length(Hcs) - length(Cards) of
						?true	-> DeskNew = desk_mod:eventing(Desk, Behavior, Seat, Cards);
						?false	-> DeskNew = SendError(?E_ADD_ERROR12)
					end,
					{?noreply, DeskNew#desk{time_last=util_time:seconds()}}
			end
	end.


%%	微信分享回调
wechat_share_cb(Desk, Mpid) ->
	#desk{bout_total=Btotal,seat_list=Seats,ext_d=SjD,ext_dm=SjDm} = Desk,
	{#sj_d{}, #sj_dm{game_mode=Mode,is_uplevel=IsLevelup,default_level=DefLevel}} = {SjD, SjDm},
	msg:send(Mpid, vipm_api:msg(?A_VIPM_SJ_WECHAT_SHARE, {Mode, Btotal, IsLevelup, DefLevel, Seats})),
	{?noreply, Desk}.


%% ====================================================================
%% ====================================================================
%% ====================================================================
%% ====================================================================

%%	开始游戏
game_on(Desk) ->
	DeskNew = Desk#desk{is_start_game=?true,is_cancle_dismiss=?true,state=?GAME_LORD,state_time=?zero},
	desk_mod:game_on(DeskNew#desk{bout_idx=DeskNew#desk.bout_idx+?one}).


%%	亮主阶段
lording(Desk) ->
	#desk{state_time=StateTime} = Desk,
	case StateTime of
		?zero	->
			desk_mod:deal_card(Desk);
		Time when Time < 15	->
			Desk#desk{state_time=Time+?one};
		StateTime	->
			desk_mod:bury_card(Desk) 
	end.


%%	出牌阶段
leading(Desk) ->
	case Desk#desk.event_ing of
		#de{event_list=Elist} when length(Elist) == ?CONST_SJ_GAME_STATE_NUM_PLAYER	-> 
			desk_mod:round_over(Desk);
		_Otherwise	-> 
			Desk
	end.


%%	游戏结束
game_over(Desk) ->
	DeskNew = desk_mod:game_over(Desk),
	ai_mod:send_vslogs_msg(DeskNew),
	DeskNew.
