%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : vipm_zpsybp_api.erl
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
-include("mod.game_zpsybp.hrl").

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
	ExtsData = ?null,
	db:mount_add(EndMsgId,?MODULE,ProcKey,ExtsData,IsOpen,IsRefresh).

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
msg(?A_VIPM_ZPSYBP_INTO_EXT, Seat)->
	Bin1 = msg_group(?A_VIPM_ZPSYBP_INTO_EXT, Seat),
	msg:msg(?A_VIPM_ZPSYBP_INTO_EXT, Bin1);


% 房间信息 [3025]
msg(?A_VIPM_ZPSYBP_ROOM_INFO, IsKeep)->
    Bin1 = msg_group(?A_VIPM_ZPSYBP_ROOM_INFO, IsKeep),
	msg:msg(?A_VIPM_ZPSYBP_ROOM_INFO, Bin1);


% 总结算 [3400]
msg(?A_VIPM_ZPSYBP_LIQUIDATION, {RoomKey, Begin, End, Round, SettelementInfos})->
	Bin1 = msg:encode([{?int32u, RoomKey}, {?int32u, Begin}, {?int32u, End}, {?int8u, Round}, {?int8u, length(SettelementInfos)}]),
	Bin2 = iolist_to_binary([msg_group(?A_VIPM_ZPSYBP_LIQUIDATION_INFO, Info) || Info <- SettelementInfos]),
	msg:msg(?A_VIPM_ZPSYBP_LIQUIDATION, <<Bin1/binary, Bin2/binary>>);


% 微信分享回复 [3451] 
msg(?A_VIPM_ZPSYBP_WECHAT_SHARE_ACK, {IsKeep, Rule, Limit, Names})->
    Bin1 = msg:encode([{?bool,IsKeep},{?int8u,Rule},{?int8u,Limit},{?int8u, length(Names)}]),
	Bin2 = iolist_to_binary([msg:encode([{?string, Name}]) || Name <- Names]),
    msg:msg(?A_VIPM_ZPSYBP_WECHAT_SHARE_ACK, <<Bin1/binary, Bin2/binary>>);


msg(MsgID,DataList)->
	?MSG_ERROR("MsgID:~p,DataList:~p",[MsgID,DataList]),
	<<>>.

%% --------------------------------------------------------------------
%% @description: 数据转2进制(协议块)
%% --------------------------------------------------------------------  	
% [协议块]玩家信息拓展 [3015]
msg_group(?A_VIPM_ZPSYBP_INTO_EXT, Seat)->
	#seat{seat_id=SeatId,is_ready=IsReady,ext_s=#zpsybp_s{hu_acc=HuAcc,hu_curr=HuCurr}} = Seat,
    msg:encode([{?int8u, SeatId}, {?bool, IsReady}, {?int16, HuAcc}, {?int16, HuCurr}]);


% [协议块]房间信息 [3025]
msg_group(?A_VIPM_ZPSYBP_ROOM_INFO, IsKeep)->
    msg:encode([{?bool, IsKeep}]);


% [协议块]总结算信息块 [3401]
msg_group(?A_VIPM_ZPSYBP_LIQUIDATION_INFO, {Seat, IsBestWinner, IsBestLoser, ProfitPoint})->
	#seat{uid=Uid,seat_id=SeatId,uname=Name,ext_s=ExtS} = Seat,
	#zpsybp_s{hu_times=HuTimes,sd_times=SelfdrawTimes,pao_times=PaoTimes,ti_times=TiTimes,hu_acc=HuAcc} = ExtS,
	msg:encode([{?int32u,Uid}, 
				{?int8u,SeatId},
				{?string,Name}, 
				{?bool,IsBestWinner},
				{?bool,IsBestLoser},
				{?int8u,HuTimes},
				{?int8u,SelfdrawTimes},
				{?int8u,PaoTimes},
				{?int8u,TiTimes},
				{?int16,HuAcc},
				{?int16,ProfitPoint}]);


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
doloop(#desk{state=?DISBAND_ROOM}=Desk) ->
	all_calculation(Desk);
doloop(Desk) ->
	case desk_api:doloop(Desk) of
		{?noreply, DeskTmp}			->
			case Desk#desk.state of
				?GAME_OVER	->
					DeskNew = gameover(DeskTmp);
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
	#desk{master_uid=MasterUid,game_id=GameId,room_id=RoomId,desk_id=DeskId,vipm_key=VipmKey,ext_d=ExtD,seat_list=Seats} = Desk,
	#zpsybp_d{player_limit=PlayerLimit} = ExtD,
	#seat{uid=Uid,uname=Uname,mpid=Mpid,icon_url=IconUrl} = Seat,
	NewSeats = [NewSeat|_] = ai_mod:seats_renew(Seats, Seat, PlayerLimit),
	case lists:keymember(Uid, #seat.uid, Seats) of
		?true	->
			msg:send(Mpid, desk_api:msg(?A_DESK_JOIN_OK, {GameId, DeskId, VipmKey, ?true})),
			desk_api:join_user_ok(Uid, self(), RoomId, DeskId, GameId),
			{?noreply, Desk#desk{seat_list=NewSeats}};
		?false	->
			case length(Seats) < PlayerLimit of
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
gameover(Desk) ->
	begin ai_mod:save_vslogs_db(Desk), Desk end.



%%	总结算 
all_calculation(Desk) ->
	#desk{master_uid=MasterUid,time_create=Begin,vipm_key=RoomKey,seat_list=Seats,bout_idx=BoutIdx} = Desk,
	{Begin, End} = {Desk#desk.time_create, util_time:seconds()},
	SoList = ai_mod:make_so_list(Seats),
	dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_update, [RoomKey, ?two, BoutIdx, SoList]), 
	Liquidations = ai_mod:make_liquidation_infos(Seats),  
	desk_api:broadcast(Desk, vipm_api:msg(?A_VIPM_ZPSYBP_LIQUIDATION, {RoomKey, Begin, End, BoutIdx, Liquidations})),
	vipm_api:notify_reflesh(MasterUid),
	{?stop, ?normal, Desk}.



%%	好友房用户日志
logs_ka_user_data(Desk) ->
	HuAccs = [util:round(HuAcc, ?ten) || #seat{ext_s=#zpsybp_s{hu_curr=HuAcc}} <- Desk#desk.seat_list],
	GetProfit = fun(MyHuAcc) -> lists:sum([util:round(MyHuAcc, ?ten) - HuAcc || HuAcc <- HuAccs]) end,
	[{Uid, GetProfit(HuAcc), ?zero, []} || #seat{uid=Uid,ext_s=#zpsybp_s{hu_curr=HuAcc}} <- Desk#desk.seat_list].
