%% @author Administrator
%% @doc @todo Add description to ai_zpsybp_mod.
-module(sybp_ai_mod).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.desk_scene.hrl").
-include("mod.game_zpsybp.hrl"). 

-compile(export_all).

-define(vip_room_id,			121).

%% ====================================================================
%% API functions
%% ====================================================================

%% --------------------------------------------------------------------
%% internal functions
%% --------------------------------------------------------------------

%%	生成一个座位
seats_renew(Seats, SeatTmp, PlayerLimit) ->
	#seat{uid=Uid,uname=Uname,skin_id=SkinId,icon_id=IconId,icon_url=IconUrl,vip_lv=VipLv,sex=Sex,lv=Lv,charm=Charm,mpid=Mpid,socket=Socket} = SeatTmp,
	case lists:keytake(Uid, #seat.uid, Seats) of
		?false	->
			SeatId = seat_id_get(PlayerLimit, [Id || #seat{seat_id=Id} <- Seats]),
			[SeatTmp#seat{seat_id=SeatId} | Seats];
		{?value, SeatOld, SeatTail}	->
			SeatNew = SeatOld#seat{uname=Uname,skin_id=SkinId,icon_id=IconId,icon_url=IconUrl,vip_lv=VipLv,sex=Sex,lv=Lv,charm=Charm,mpid=Mpid,socket=Socket},
			[SeatNew | SeatTail]
	end.


%%	检查牌事件
check_event(HandDeal, OpenDeal, PassDeal, FocusCard, Events) ->
	{HDFlatten, ODFlatten} = {lists:flatten(HandDeal), OpenDeal},
	Fun = fun (Event) -> {Event, sybp_ai_api:check_event(Event, HDFlatten, ODFlatten, FocusCard, PassDeal)} end,
	[{Event, Deal} || {Event, Deal} <- lists:map(Fun, Events), Deal =/= [], Deal =/= ?null].


%%	检查胡牌
%%	提跑后胡
check_hu(HandDeal, OpenDeal, RealHuCard) ->
	GuessCard = hd([Card || Card <- lists:flatten(HandDeal), Card =/= []]),
	HandDealNew = sybp_ai_api:deep_rember([GuessCard], HandDeal),
	check_hu(HandDealNew, OpenDeal, RealHuCard, GuessCard).

%%	直接胡
check_hu(HandDeal, OpenDeal, RealHuCard, GuessCard) ->
	case sybp_ai_api:event_handler(HandDeal, OpenDeal, GuessCard, ?CONST_ZPSYBP_CARD_STATE_WIN) of
		[]		-> [];
		HuDeal	->
			{Opens, Hus} = lists:split(length(OpenDeal), lists:reverse(HuDeal)),
			Hus ++ Opens ++ [[RealHuCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_WIN}]]
	end.



%%	处理事件
handle_event(Desk, Seat, [HuCard], ?CONST_ZPSYBP_CARD_STATE_WIN) ->
	#desk{banker_idx=BankerIdx,seat_list=Seats,event_wait=DesWait,lead_heap=Discarded,ext_d=ExtD,ext_dm=ExtDm} = Desk,
	#zpsybp_d{focus_idx=FocusIdx,hu_data=HuDealOld} = ExtD,
	#seat{seat_id=SeatId,ext_s=ExtS} = Seat,
	#zpsybp_s{hand_deal=HdO,open_deal=OdO,sd_times=SdtO,hu_times=HtO,hu_acc=HaO} = ExtS,
	MyDe = lists:keyfind(SeatId, #de.seat_idx, DesWait),
	GuessCard = HuCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_NORMAL},
	case auto_event_check([MyDe]) of 
		?null					-> 
			HuDeal = ?IF(HuDealOld == ?null, check_hu(HdO, OdO, HuCard, GuessCard), HuDealOld);
		{SeatId, State, Deal}	->
			{HdN, OdN, _} = sybp_ai_api:event_handler(HdO, OdO, [Deal], State),	
			case check_hu(HdN, OdN, HuCard) of
				[]		->
					HuDeal = check_hu(HdO, OdO, HuCard, GuessCard);
				HuDeal	-> 
					sybp_desk_mod:handle_event(Desk#desk{event_wait=[MyDe],event_ing=MyDe}, Seat, {State, Deal})
			end
	end,
	case Discarded of
		[]	-> 	
			HuData = ?naturl;
		[_]	-> 
			HuData = ?unnatuel;
		Discarded when FocusIdx == SeatId	-> 
			HuData = ?selfdraw;
		Discarded	->
			HuData = ?justhu
	end,
	HuCurr = sybp_ai_api:profit_curr_get(HuDeal) + element(?two, HuData),
	{HaN, HtN, SdtN} = {HaO + HuCurr, HtO + ?one, ?IF(HuData == ?selfdraw, SdtO + ?one, SdtO)},
	SeatNew = Seat#seat{ext_s=ExtS#zpsybp_s{hand_deal=[],open_deal=HuDeal,hu_curr=HuCurr,hu_acc=HaN,hu_times=HtN,sd_times=SdtN}},
	SeatsTmp = [Seat#seat{ext_s=ExtS#zpsybp_s{hu_curr=?zero}} || #seat{ext_s=ExtS}=Seat <- Seats],
	SeatsNew = lists:keyreplace(SeatId, #seat.seat_id, SeatsTmp, SeatNew),
	#seat{ext_s=#zpsybp_s{hu_acc=LastBankerHuAcc}} = lists:keyfind(BankerIdx, #seat.seat_id, Seats),
	{ExtDNew, ExtDmNew} = {ExtD#zpsybp_d{hu_data={SeatId,HuData}}, ExtDm#zpsybp_dm{last_banker_profit=LastBankerHuAcc}},
	Desk#desk{state=?GAME_OVER,state_time=?zero,event_wait=[],event_ing=?null,seat_list=SeatsNew,ext_d=ExtDNew,ext_dm=ExtDmNew};
handle_event(Desk, Seat, Deal, State) ->
	#desk{seat_list=Seats,delay=Delays,curr_seat_idx=LastIdx,ext_d=ExtD} = Desk,
	#zpsybp_d{focus_method=Method} = ExtD,
	#seat{seat_id=SeatId,ext_s=ExtS,mpid=Mpid} = Seat,
	#zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,ti_times=TiTimes,pao_times=PaoTimes} = ExtS,	
	Deals = ?IF(State == ?CONST_ZPSYBP_CARD_STATE_CHI, util:lists_split(Deal, ?three), [Deal]),
%% 	Deals = ?IF(State == ?CONST_ZPSYBP_CARD_STATE_CHI, ai_zpsybp_api:append_by(fun(_, Deal) -> length(Deal) < ?three end, Deal, []), [Deal]),
	{HandDealNew, OpenDealNew, DeskState} = sybp_ai_api:event_handler(HandDeal, OpenDeal, Deals, State),
	HuCurr = sybp_ai_api:profit_hide_find(HandDealNew) + sybp_ai_api:profit_curr_get(OpenDealNew),
	TiTimesNew = ?IF(lists:member(State, ?TIS), TiTimes + ?one, TiTimes),
	PaoTimesNew = ?IF(lists:member(State, ?PAOS), PaoTimes + ?one, PaoTimes),
	SeatNew = Seat#seat{ext_s=ExtS#zpsybp_s{hand_deal=HandDealNew,open_deal=OpenDealNew,hu_curr=HuCurr,ti_times=TiTimesNew,pao_times=PaoTimesNew}},
	msg:send(Mpid, sybp_vipm_api:msg(?A_VIPM_ZPSYBP_INTO_EXT, SeatNew)),
	SeatsTmp = lists:keyreplace(SeatId, #seat.seat_id, Seats, SeatNew),
	case State == ?CONST_ZPSYBP_CARD_STATE_WPAO andalso Method == ?discard of
		?false	->
			SeatsNew = SeatsTmp;
		?true	->
			{?value, #seat{ext_s=DisExtS}=Discarder, Tail} = lists:keytake(LastIdx, #seat.seat_id, SeatsTmp),
			DisPass = [Card#bpc{state=State} || Card <- sybp_ai_api:get_pai_case(), State <- ?PASS],
			SeatsNew = [Discarder#seat{ext_s=DisExtS#zpsybp_s{pass_deal=DisPass}} | Tail]
	end,
	ExtDNew = ExtD#zpsybp_d{focus_card=[],focus_method=?alt_pointer},
	DeskNew = Desk#desk{event_wait=[],event_ing=?null,state=DeskState,seat_list=SeatsNew,curr_seat_idx=SeatId,ext_d=ExtDNew},
	case Method == ?discard andalso lists:member(State, ?PAOS) of
		?true	->
			DeskNew#desk{delay=[{fun sybp_ai_mod:send_event_msg/2, [SeatId, Deals], ?one} | Delays]};
		?false	->
			Satis = DeskState == ?DISCARDING andalso not lists:member(State, ?AUTO_EVENTS), 
			EventMsg = ?IF(Satis, sybp_desk_api:msg(?A_DESK_ZPSYBP_DISCARD, {SeatId, ?alt_pointer, []}), <<>>),
			begin desk_api:broadcast(Desk, EventMsg), send_event_msg(SeatId, Deals), DeskNew end
	end.


%% --------------------------------------------------------------------
%% free functions
%% --------------------------------------------------------------------

%%	获取空闲座位id
seat_id_get(Limit, OldIds) ->
	Ids = lists:seq(1, Limit) -- OldIds,
	?IF(?EMPTY_ARRAY(Ids), ?zero, hd(Ids)).


%%	随机庄家的座位id
rand_banker_idx_get(Limit) ->
	util_rand:uniform(Limit).


%%	获取下家的座位id
next_seat_id_get(Id, Limit) when is_number(Limit) ->
	?IF(Id == Limit, ?one, Id + ?one).


%%	获取上家的座位id
last_seat_id_get(Id, Limit) ->
	?IF(Id == ?one, Limit, Id - ?one).


%%	获取最后执行的事件
last_event_get(Desk) ->
	#desk{event_finish=EventFinish} = Desk,
	case EventFinish of
		[]				-> 
			?null;
		[De|_Events]	->
			element(?one, hd(De#de.event_list))
	end.


%%	搜索自动处理事件
auto_event_check([]) ->
	?null;
auto_event_check(Des) ->
	EventsOld = lists:append([[{I, E, D} || {E, D} <- Es] || #de{event_list=Es,seat_idx=I} <- Des]),
	EventsNew = lists:sort(fun({_, E1, _}, {_, E2, _}) -> E1 > E2 end, EventsOld),
	Pred = fun({I, E, D}) -> ?IF(lists:member(E, ?AUTO_EVENTS), {I, E, D}, ?false) end,
	case sybp_ai_api:any(Pred, EventsNew) of
		[]		-> ?null;
		Stuff	-> auto_event_check(Stuff, [I || {I, ?CONST_ZPSYBP_CARD_STATE_WIN, _} <- EventsNew])
	end.

auto_event_check({Idx, Event, Deal}, []) ->
	{Idx, Event, Deal};
auto_event_check({Idx, Event, Deal}, HuIdxs) ->
	case lists:member(Event, ?PAOS) of
		?false	->
			{Idx, Event, Deal};
		?true	->
			case lists:delete(Idx, HuIdxs) of
				[]	->		%%	TODO check hu after pao
					?null;
				_	->
					?null
			end
	end.			


%%	检查事件优先级(是否优先度最高)
priority_event_check(_Des, ?null, _Limit) ->
	?false;
priority_event_check([], _De, _Limit) ->
	?true;
priority_event_check(Des, #de{event_list=[{State, _}],seat_idx_target=FocusIdx,seat_idx=CurrIdx}, Limit) ->
	AllEvents = lists:append([[{Idx, Event} || {Event, _} <- Events] || #de{event_list=Events,seat_idx=Idx} <- Des]),
	MyRelativeIdx = ?IF(CurrIdx < FocusIdx, CurrIdx + Limit, CurrIdx),
	Pred = fun(Idx) -> MyRelativeIdx < ?IF(Idx < FocusIdx, Idx + Limit, Idx) end,
	lists:all(fun({Idx, Event}) -> State > Event orelse State == Event andalso Pred(Idx) end, AllEvents).


%%	处理延时任务
handle_delay_action([{Fun, Args, ?zero} | Actions]) ->
	begin apply(Fun, Args), handle_delay_action(Actions) end;
handle_delay_action([{Fun, Args, Sec} | Actions]) ->
	[{Fun, Args, Sec - 1} | handle_delay_action(Actions)];
handle_delay_action([]) ->
	[].


%% --------------------------------------------------------------------
%% msg functions
%% --------------------------------------------------------------------
%%	生成房间拓展数据
make_so_list([Seat|Seats]) ->
	#seat{uid=Uid,uname=Uname,icon_url=IconUrl,ext_s=#zpsybp_s{hu_acc=HuAcc}} = Seat,
	So = #so{uid=Uid,uname=Uname,icon_url=IconUrl,chip=HuAcc},
	[So|make_so_list(Seats)];
make_so_list([]) ->
	[].

%%	生成总结算具体参数
make_liquidation_infos(Seats) ->
	SeatsNew = [Seat#seat{ext_s=ExtS#zpsybp_s{hu_acc=util:round(HuAcc, ?ten)}} || #seat{ext_s=#zpsybp_s{hu_acc=HuAcc}=ExtS}=Seat <- Seats],
	HuAccs = [HuAcc || #seat{ext_s=#zpsybp_s{hu_acc=HuAcc}} <- SeatsNew],
	[make_liquidation_infos(Seat, HuAccs) || Seat <- SeatsNew].

make_liquidation_infos(Seat, HuAccs) ->
	#seat{ext_s=#zpsybp_s{hu_acc=MyHuAcc}} = Seat,
	{Seat, lists:max(HuAccs) == MyHuAcc, lists:min(HuAccs) == MyHuAcc, lists:sum([MyHuAcc - HuAcc || HuAcc <- HuAccs])}.

%%	生成一组空牌
make_empty_deal(Deal) ->
	[Card#bpc{kind=?zero,point=?zero} || Card <- Deal].

%%	生成一组暗牌
make_dark_deal([Card|Tail]) ->
	[Card#bpc{kind=?zero,point=?zero} || Card <- Tail] ++ [Card].

%%	生成桌子信息
make_desk_msg(Desk) ->
	#desk{master_uid=MasterUid,bout_idx=BoutIdx,ext_dm=#zpsybp_dm{satis_keep_banker=IsKeep}} = Desk,
	vipm_api:msg_update(Desk, ?zero, BoutIdx, MasterUid, IsKeep).

%%	生成座位信息
make_seat_msg(Desk, Seat) ->
	vipm_api:msg_into(Desk, Seat, Seat).

%% 生成投票信息
make_vote_msg(#vote_info{creator_uid=VoterUid,end_time=End,v_list=VList}, Uid) when VoterUid =/= ?zero ->
	State = lists:keymember(?one, Uid, VList),
	Left = End - util_time:seconds(),
	Bin1 = msg:encode([{?bool, ?true}]),
	Bin2 = vipm_api:msg_group(?A_VIPM_VOTE, {VoterUid, Left, State, VList}),
	<<Bin1/binary, Bin2/binary>>;
make_vote_msg(_Else, _Uid) ->
	msg:encode([{?bool, ?false}]).
	
%% 生成可能触发的事件信息
make_maybe_msg(#de{event_list=Events}, FocusCard, MsgType) when is_record(FocusCard, bpc) ->
	PassEvent = {?CONST_ZPSYBP_CARD_STATE_CHOU, [FocusCard]},
	EventsNew = [{Event, Deal} || {Event, Deal} <- Events, not lists:member(Event, ?AUTO_EVENTS)],
	desk_zpsybp_api:MsgType(?A_DESK_ZPSYBP_MAYBE_EVENT, ?IF(EventsNew == [], [], [PassEvent|EventsNew]));
make_maybe_msg(_De, _FocusCard, _MsgType) ->
	<<?zero>>.

%%	发送可能触发的事件信息
send_maybe_msg(Des, FocusCard) ->
	[msg:send(Mpid, make_maybe_msg(De, FocusCard, ?msg)) || #de{mpid=Mpid}=De <- Des].

%%	发送触发事件信息块
send_event_msg(_SeatId, []) ->
	?ok;
send_event_msg(SeatId, [Deal | Deals]) when is_list(Deal) ->
	begin send_event_msg(SeatId, Deal, (hd(Deal))#bpc.state), send_event_msg(SeatId, Deals) end.

send_event_msg(SeatId, Deal, State) 
  when State == ?CONST_ZPSYBP_CARD_STATE_WEI; State == ?CONST_ZPSYBP_CARD_STATE_WTI; State == ?CONST_ZPSYBP_CARD_STATE_TI ->
	desk_api:broadcast(self(), sybp_desk_api:msg(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, ?eventing, [make_dark_deal(Deal)]}));
send_event_msg(SeatId, Deal, ?CONST_ZPSYBP_CARD_STATE_WEI) ->
	desk_api:broadcast(self(), sybp_desk_api:msg(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, ?eventing, [make_empty_deal(Deal)]}));
send_event_msg(SeatId, Deal, _State) ->
	desk_api:broadcast(self(), sybp_desk_api:msg(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, ?eventing, [Deal]})).

%%	发送对战日志信息
send_vslogs_msg(Desk) ->
	#desk{game_id=GameId,desk_id=DeskId,vipm_key=RoomKey,room_type=RoomType,bout_idx=BoutIdx,seat_list=Seats,time_start=Begin,time_end=Finish} = Desk,
	GetProfit = fun(MyHuAcc) -> lists:sum([MyHuAcc - util:round(HuAcc, ?ten) || #seat{ext_s=#zpsybp_s{hu_acc=HuAcc}} <- Seats])end,
	VsLogs = [{BoutIdx, Begin, [{Uid, [{?CONST_VGOODS_SCORE, ExtS#zpsybp_s.hu_curr}]} || #seat{uid=Uid,ext_s=ExtS} <- Seats]}],
	Send = fun(Mpid, HuAcc) -> msg:send(Mpid, vipm_api:msg(?A_VIPM_LOGS_INFO, {GameId, DeskId, RoomKey, Finish, RoomType, GetProfit(HuAcc), VsLogs})) end,
	[Send(Mpid, util:round(HuAcc, ?ten)) || #seat{mpid=Mpid,ext_s=#zpsybp_s{hu_acc=HuAcc}} <- Seats].

%%	保存对战日志到数据库
save_vslogs_db(Desk) ->
	#desk{seat_list=Seats} = Desk,
	PublicDatas = [{Uid, [{?CONST_VGOODS_SCORE, ExtS#zpsybp_s.hu_curr}]} || #seat{uid=Uid,ext_s=ExtS} <- Seats],
	PrivateDatas = [{Uid, [{?CONST_VGOODS_SCORE, ExtS#zpsybp_s.hu_acc}]} || #seat{uid=Uid,ext_s=ExtS} <- Seats],
	vipm_api:logs_ka_about(Desk, PublicDatas, PrivateDatas, ?zero, [], <<>>). 
	
%%	保存胜利失败局数
add_type_data_value([#seat{uid=Uid,seat_id=WinId} | Seats], WinId) ->
	user_api:add_type_data_value(Uid, {?CONST_GAME_ID_ZPSYBP, ?CONST_TD_ADD_WIN_COUNT}, ?one),
	add_type_data_value(Seats, WinId);
add_type_data_value([#seat{uid=Uid} | Seats], WinId) ->
	user_api:add_type_data_value(Uid, {?CONST_GAME_ID_ZPSYBP, ?CONST_TD_ADD_FAIL_COUNT}, ?one),
	add_type_data_value(Seats, WinId);
add_type_data_value([], _WinSeatId) ->
	?ok.
