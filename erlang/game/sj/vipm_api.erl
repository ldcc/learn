%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : vipm_sj_api.erl
%%% Created : 
%%% -------------------------------------------------------------------
-module(vipm_api).

-behaviour(gamecore_api).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.desk_scene.hrl").  
-include("mod.game_sj.hrl").

%% --------------------------------------------------------------------
%% 以下系统默认导出(勿删)
-export([gc_mount/2,gc_switch/1,gc_callback_start/1,gc_callback_stop/0,msg/2,msg_group/2,data_get/1,data_set/2,cache_get/1,cache_set/2]).

%% 扩展导出
-export([
		 doloop/1,
		 vipm_join/2,
		 all_calculation/1,
		 logs_ka_user_data/1
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
	SJsData = ?null,
	db:mount_add(EndMsgId,?MODULE,ProcKey,SJsData,IsOpen,IsRefresh).

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
% 玩家信息 [3015]
msg(?A_VIPM_SJ_SEAT_INFO, Seat)->
	Bin1 = msg_group(?A_VIPM_SJ_SEAT_INFO, Seat),
	msg:msg(?A_VIPM_SJ_SEAT_INFO, Bin1);

% 房间信息 [3025]
msg(?A_VIPM_SJ_ROOM_INFO, {Mode, Level, Data, IsLevelup})->
    Bin1 = msg_group(?A_VIPM_SJ_ROOM_INFO, {Mode, Level, Data, IsLevelup}),
	msg:msg(?A_VIPM_SJ_ROOM_INFO, Bin1);

% 对战总结算 [3400]
msg(?A_VIPM_SJ_LIQUIDATION, Desk)->
	#desk{seat_list=Seats,vipm_key=DeskKey,time_create=Begin,time_end=Over,bout_idx=BoutCurr,bout_total=BoutAcc} = Desk,
    Bin1 = msg:encode([{?int32u, DeskKey}, {?int32u, Begin}, {?int32u, Over}, {?int8u, BoutCurr}, {?int8u, BoutAcc}, {?int8u, length(Seats)}]),
	Bin2 = iolist_to_binary([msg_group(?A_VIPM_SJ_LIQUIDATE_INFO, Seat) || Seat <- Seats]),
    msg:msg(?A_VIPM_SJ_LIQUIDATION, <<Bin1/binary, Bin2/binary>>);

% 微信分享回复 [3451]
msg(?A_VIPM_SJ_WECHAT_SHARE,{Mode, Btotal, IsLevelup, DefLevel, Names})->
    Bin1 = msg:encode([{?int8u, Mode}, {?int8u, Btotal}, {?int8u, IsLevelup}, {?int8u, DefLevel}, {?int8u, length(Names)}]),
	Bin2 = iolist_to_binary([msg:encode([{?string, Name}]) || Name <- Names]),
    msg:msg(?A_VIPM_SJ_WECHAT_SHARE, <<Bin1/binary, Bin2/binary>>);

msg(MsgID,DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>.

%% --------------------------------------------------------------------
%% @description: 数据转2进制(协议块)
%% --------------------------------------------------------------------  	
% [协议块]玩家信息拓展 [3015]
msg_group(?A_VIPM_SJ_SEAT_INFO, Seat)->
	#seat{seat_id=SeatId,is_ready=IsReady,ext_s=#sj_s{level=Level}} = Seat,
    msg:encode([{?int8u, SeatId}, {?bool, IsReady}, {?int8, Level}]);

% [协议块]房间信息 [3025]
msg_group(?A_VIPM_SJ_ROOM_INFO, {CardNum, Mode, Level, Data, IsLevelup})->
	msg:encode([{?int8u, CardNum}, {?int8u, Mode}, {?int8u, Level}, {?int8u, Data}, {?bool, IsLevelup}]);	

% [协议块]玩家总结算信息块 [3401] 
msg_group(?A_VIPM_SJ_LIQUIDATE_INFO, Seat)->
	#seat{uid=Uid,ext_s=#sj_s{score=Score,tractor_times=Ttimes,defeat_times=Dtimes,lord_times=Ltimes,win_times=Wtimes}} = Seat,
	msg:encode([{?int32u, Uid}, {?int32, Score},{?int8u, Ttimes}, {?int8u, Dtimes}, {?int8u, Ltimes}, {?int8u, Wtimes}]);

msg_group(MsgID,DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>.

%% Data数据get
%%  @返回 Data
data_get(Gate) ->
	TagEtsMod = xxx_ets,
	InitData  = ?init,
	app_tool:data_get(Gate, TagEtsMod, InitData).
%% Data数据set
%%  @返回 Gate
data_set(Gate,Data)->
	TagEtsMod = xxx_ets,
	app_tool:data_set(Gate, TagEtsMod, Data).

%% Cache数据get
%%  @返回 Data
cache_get(Gate) ->
	TagEtsMod = xxx_ets,
	InitData  = ?init,
	app_tool:cache_get(Gate,TagEtsMod,InitData).
%% Cache数据set
%%  @返回 Gate
cache_set(Gate,Data)->
	TagEtsMod = xxx_ets,
	app_tool:cache_set(Gate, TagEtsMod,Data).


%% ====================================================================
%% Internal functions
%% ====================================================================

%% doloop(Desk) when Desk#desk.vipm_key /= 706548 ->
%% 	{?stop, ?normal, Desk};
doloop(#desk{state=?GAME_DISBAND}=Desk) ->
	all_calculation(Desk);
doloop(Desk) ->
	case desk_api:doloop(Desk) of
		{?noreply, DeskTmp}			->
			case Desk#desk.state of
				?GAME_ON	->
					DeskNew = game_on(DeskTmp);
				?GAME_OVER	->
					DeskNew = game_over(DeskTmp);
				_DeskState	-> 
					DeskNew = DeskTmp
			end,
			{?noreply, DeskNew};
		{Action, Reason, DeskNew}	->
			?IF(Reason == ?normal, ?skip, ?MSG_ERROR("~nAction::~p, Reason::~p~n", [Reason])),
			{Action, Reason, DeskNew}
	end.


%%	加入房间
vipm_join(Desk, Seat)->
	#desk{master_uid=MasterUid,game_id=GameId,room_id=RoomId,desk_id=DeskId,vipm_key=VipmKey,seat_list=Seats} = Desk,
	#seat{uid=Uid,uname=Uname,mpid=Mpid,icon_url=IconUrl} = Seat,
	NewSeats = [NewSeat|_] = ai_mod:seats_renew(Seats, Seat#seat{ext_s=#sj_s{level=Desk#desk.ext_dm#sj_dm.default_level}}),
	case lists:keymember(Uid, #seat.uid, Seats) of
		?true	->
			msg:send(Mpid, desk_api:msg(?A_DESK_JOIN_OK, {GameId, DeskId, VipmKey, ?true})),
			desk_api:join_user_ok(Uid, self(), RoomId, DeskId, GameId),
			{?noreply, Desk#desk{seat_list=NewSeats}};
		?false	->
			case length(Seats) < ?CONST_SJ_GAME_STATE_NUM_PLAYER of
				?true	->
					msg:send(Mpid, desk_api:msg(?A_DESK_JOIN_OK, {GameId, DeskId, VipmKey, ?false})),
					desk_api:broadcast(Desk, ai_mod:make_seat_msg(Desk, NewSeat)),
					desk_api:broadcast(Desk, ai_mod:make_desk_msg(Desk)),					
					desk_api:join_user_ok(Uid, self(), RoomId, DeskId, GameId),
					UidInfo = #so{uid=Uid,uname=Uname,icon_url=IconUrl},
					dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_add, [VipmKey, UidInfo]),
					vipm_api:notify_reflesh(MasterUid),
					{?noreply, Desk#desk{seat_list=NewSeats}};
				?false	->
					system_api:send_error(Mpid, ?E_PRIVATE_ROOM_HAS_FULL),
					{?noreply, Desk}
			end
	end.


%%	游戏结束
game_on(Desk) ->
	begin vipm_api:notify_reflesh(Desk#desk.master_uid), Desk end.


%%	游戏结束
game_over(Desk) ->
	begin ai_mod:save_vslogs_db(Desk), Desk end.


%%	总结算 
all_calculation(Desk) ->
	#desk{master_uid=MasterUid,vipm_key=RoomKey,seat_list=Seats,bout_idx=BoutIdx,ext_dm=SjDm} = Desk,
	#sj_dm{game_mode=GameMode,multiple=Multiple} = SjDm,
	SeatsNew = ai_mod:liq_score(GameMode, Multiple, Seats),
	SoList = ai_mod:make_so_data(SeatsNew),
	dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_update, [RoomKey, ?two, BoutIdx, SoList]), 
	vipm_api:notify_reflesh(MasterUid),
	desk_api:broadcast(Desk, vipm_api:msg(?A_VIPM_SJ_LIQUIDATION, Desk#desk{seat_list=SeatsNew})),
	{?stop, ?normal, Desk#desk{seat_list=SeatsNew,state=?GAME_PREP}}.


%%	好友房用户日志
logs_ka_user_data(Desk) ->
	#desk{seat_list=Seats,ext_dm=#sj_dm{game_mode=Mode}} = Desk,
	MakeOffset = fun(SjS) -> ?IF(Mode == ?bout, SjS#sj_s.offset_score, SjS#sj_s.offset_level) end,
	[{Uid, MakeOffset(SjS), ?zero, []} || #seat{uid=Uid,ext_s=SjS} <- Seats].
