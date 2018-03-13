%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : desk2_mod.erl
%%% Created : 2008.06.20
%%% Description: 
%%% -------------------------------------------------------------------
-module(desk_mod).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.desk_scene.hrl").  
-include("mod.game_zpsybp.hrl").

-export([
		 game_init/1,
		 flashcard_begin/1,
		 flashcard_over/1,
		 drawing/1,
		 receiving/1,
		 handle_event/3,
		 game_over/1
		]).
%% --------------------------------------------------------------------
%% game function
%% --------------------------------------------------------------------

%%	初始化游戏
game_init(Desk) ->
	#desk{vipm_key=RoomKey,ext_d=ExtD,seat_list=Seats,banker_idx=OldBankerIdx,bout_idx=BoutIdx} = Desk,
	#zpsybp_d{player_limit=PlayerLimit} = ExtD,
	SoList = ai_mod:make_so_list(Seats),	
	dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_update, [RoomKey, ?one, BoutIdx, SoList]),
	BankerIdx= ?IF(OldBankerIdx == ?zero, ai_mod:rand_banker_idx_get(PlayerLimit), OldBankerIdx),
	{Orders, CardHeap} = ai_api:fapai(length(Seats)),
	{?value, Banker, SeatTail} = lists:keytake(BankerIdx, #seat.seat_id, Seats),
	NewSeats = fapai(lists:zip([Banker|lists:keysort(#seat.uid, SeatTail)], Orders)),
	NewExtD = ExtD#zpsybp_d{hu_data=?null},
	Desk#desk{curr_seat_idx=BankerIdx,ext_d=NewExtD,seat_list=NewSeats,banker_idx=BankerIdx,card_heap=CardHeap,lead_heap=[]}.



%%	发牌
fapai([{Seat, {Od, Hd}}|Tail]) ->
	#seat{ext_s=ExtS,mpid=Mpid,seat_id=SeatId} = Seat,
	HuCurr = ai_api:profit_hide_find(Hd) + ai_api:profit_curr_get(Od),
	ExtSNew = ExtS#zpsybp_s{hu_curr=HuCurr,hand_deal=Hd,open_deal=Od,pass_deal=[],ban_deal=[]},
	NewSeat = Seat#seat{is_game=?true,is_ready=?false,ext_s=ExtSNew},
	msg:send(Mpid, desk_api:msg(?A_DESK_ZPSYBP_DEAL_GROUP, {SeatId, ?dealing, Hd ++ Od})),
	[NewSeat | fapai(Tail)];
fapai([]) ->
	[].



%%	亮张
flashcard_begin(Desk) ->
	#desk{seat_list=Seats,delay=Delays} = Desk,
	Fun = fun(Seat, {DelayAcc, DeAcc}) ->
				  #seat{seat_id=SeatId,mpid=Mpid,ext_s=#zpsybp_s{open_deal=OpenDeal}} =Seat,
				  Fun = fun() -> 
								msg:send(Mpid, vipm_api:msg(?A_VIPM_ZPSYBP_INTO_EXT, Seat)),
								?IF(OpenDeal == [], ?skip, ai_mod:send_event_msg(SeatId, OpenDeal))
						end,
				  Events = [{?CONST_ZPSYBP_CARD_STATE_TI, Deal} || Deal <- OpenDeal],
				  {[{Fun, [], ?two} | DelayAcc], [#de{mpid=Mpid,seat_idx=SeatId,seat_idx_target=SeatId,event_list=Events} | DeAcc]}
		  end,
	{DelaysNew, EventsFinish} = lists:foldl(Fun, {[], []}, Seats),
	Desk#desk{event_finish=EventsFinish,delay=DelaysNew++Delays}.

flashcard_over(Desk) ->
	#desk{state=DeskState,seat_list=Seats,banker_idx=BankerIdx,card_heap=CardHeap,ext_d=ExtD,bout_idx=BoutIdx} = Desk,	
	#seat{mpid=Mpid,ext_s=#zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal}} = lists:keyfind(BankerIdx, #seat.seat_id, Seats),
	FlashCard = util_rand:rand_list(util_rand:rand_list(HandDeal)),
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_ZPSYBP_BANKER_INFO, {BankerIdx, BoutIdx, CardHeap, DeskState})),
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_ZPSYBP_DISCARD, {BankerIdx, ?flashcard, [FlashCard]})),	
	ExtDNew = ExtD#zpsybp_d{focus_card=FlashCard,focus_method=?flashcard},
	case ai_mod:check_hu(HandDeal, OpenDeal, FlashCard) of
		[]		-> 
			Desk#desk{state=?DISCARDING,ext_d=ExtDNew};
		HuDeal	->
			Events = [{?CONST_ZPSYBP_CARD_STATE_WIN, [FlashCard]}],				
			WinEvent = [#de{event_list=Events,seat_idx=BankerIdx,seat_idx_target=BankerIdx,mpid=Mpid}],
			Desk#desk{state=?RECEIVING,ext_d=ExtDNew#zpsybp_d{hu_data=HuDeal},event_wait=WinEvent}
	end.



%%	等待接收事件ing
receiving(Desk) ->
	#desk{curr_seat_idx=CurrIdx,seat_list=Seats,event_wait=EventsWait,state_time=StateTime,event_ing=Eventing,ext_d=ExtD} = Desk,
	#zpsybp_d{player_limit=Limit,focus_card=FocusCard,focus_method=Method} = ExtD,
	case ai_mod:priority_event_check(EventsWait, Eventing, Limit) of
		?true	->
			#de{seat_idx=SeatIdx,event_list=[{State, Deal}]} = Eventing,
			ai_mod:handle_event(Desk, lists:keyfind(SeatIdx, #seat.seat_id, Seats), Deal, State);
		?false	-> 
			case ai_mod:auto_event_check(EventsWait) of
				?null				->
					case StateTime of 
						?zero	-> begin ai_mod:send_maybe_msg(EventsWait, FocusCard), Desk#desk{state_time=?one} end;
						_Time	-> Desk
					end;
				{Idx, State, Deal}	->
					DeskNew = handle_event(Desk, lists:keyfind(Idx, #seat.seat_id, Seats), {State, Deal}),
					#desk{seat_list=SeatsNew,state=DeskState,delay=Delays} = DeskNew,
					#seat{mpid=Mpid,ext_s=#zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal}} = lists:keyfind(Idx, #seat.seat_id, SeatsNew),
					HuDeal = ai_mod:check_hu(HandDeal, OpenDeal, FocusCard),
					case Method == ?discard orelse HuDeal == [] of
						?true	->
							EventMsg = desk_api:msg(?A_DESK_ZPSYBP_DISCARD, {Idx, ?alt_pointer, []}),
							Delay = {fun desk_api:broadcast/2, [self(), EventMsg], ?one},
							DeskNew#desk{delay=?IF(DeskState==?DISCARDING, [Delay | Delays], Delays)};
						?false	-> 
							Events = [{?CONST_ZPSYBP_CARD_STATE_WIN, [FocusCard]}],
							WinEvent = [#de{event_list=Events,seat_idx=Idx,seat_idx_target=CurrIdx,mpid=Mpid}],
							ExtDNew = ExtD#zpsybp_d{hu_data=HuDeal},
							ai_mod:send_maybe_msg(WinEvent, FocusCard),
							DeskNew#desk{state=?RECEIVING,state_time=?one,event_wait=WinEvent,ext_d=ExtDNew,state_tag=DeskState}
					end
			end
	end.


%%	荒庄
drawing(#desk{seat_list=Seats,banker_idx=BankerIdx,card_heap=[],ext_d=ExtD,ext_dm=ExtDm}=Desk) ->
	{?value, Seat, SeatsTail} = lists:keytake(BankerIdx, #seat.seat_id, Seats),
	#seat{ext_s=#zpsybp_s{hu_acc=HuAcc}=ExtS} = Seat,
	HuCurr = element(?two, ?barren),
	HuAccNew = HuAcc + HuCurr,
	SeatsTmp = [Seat#seat{ext_s=ExtS#zpsybp_s{hu_curr=?zero}} || #seat{ext_s=ExtS}=Seat <- SeatsTail],
	SeatsNew = [Seat#seat{ext_s=ExtS#zpsybp_s{hu_acc=HuAccNew,hu_curr=HuCurr}} | SeatsTmp],
	{ExtDNew, ExtDmNew} = {ExtD#zpsybp_d{hu_data={BankerIdx,?barren}}, ExtDm#zpsybp_dm{last_banker_profit=HuAcc}},
	Desk#desk{seat_list=SeatsNew,state=?GAME_OVER,ext_d=ExtDNew,ext_dm=ExtDmNew};
%%	摸牌
drawing(Desk) ->
	#desk{curr_seat_idx=LastIdx,ext_d=ExtD,event_finish=EventsFinish,card_heap=[DrawCard|TailHeap]} = Desk,
	CurrIdx = ai_mod:next_seat_id_get(LastIdx, Desk#desk.ext_d#zpsybp_d.player_limit),
	ExtDNew = ExtD#zpsybp_d{focus_idx=CurrIdx,focus_card=DrawCard,focus_method=?drawcard},
	De = #de{seat_idx=CurrIdx,seat_idx_target=CurrIdx,event_list=[{?CONST_ZPSYBP_CARD_STATE_NORMAL,[DrawCard]}]},
	draw_event(Desk#desk{ext_d=ExtDNew,event_finish=[De|EventsFinish],card_heap=TailHeap}, CurrIdx, DrawCard).
	
%%	摸牌事件
draw_event(Desk, CurrIdx, DrawCard) ->
	#desk{seat_list=Seats,delay=Delays} = Desk,
	?MSG_ECHO("~nDrawCard::~p~n", [DrawCard]),
	NextIdx = ai_mod:next_seat_id_get(CurrIdx, Desk#desk.ext_d#zpsybp_d.player_limit),
	Fun = fun(_Seat, {Des, ?true}) ->
				  {Des, ?true};
			 (#seat{seat_id=SeatIdx,mpid=Mpid,ext_s=ExtS}, {Des, _HasAuto}) when SeatIdx == CurrIdx ->
				  #zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,pass_deal=PassDeal,ban_deal=BanDeal} = ExtS,
				  Events = ai_mod:check_event(HandDeal, OpenDeal, PassDeal ++ BanDeal, DrawCard, ?DRAW_SELF_EVENTS),
				  HasAuto = lists:any(fun({Event, _}) -> lists:member(Event, ?WEIS ++ ?TIS) end, Events),
				  {[#de{mpid=Mpid,event_list=Events,seat_idx=CurrIdx,seat_idx_target=CurrIdx}|Des], HasAuto};
			 (#seat{seat_id=SeatIdx,mpid=Mpid,ext_s=ExtS}, {Des, HasAuto}) when SeatIdx == NextIdx ->
				  #zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,pass_deal=PassDeal,ban_deal=BanDeal} = ExtS,
				  Events = ai_mod:check_event(HandDeal, OpenDeal, PassDeal ++ BanDeal, DrawCard, ?DRAW_NEXT_EVENTS),
				  {[#de{mpid=Mpid,event_list=Events,seat_idx=NextIdx,seat_idx_target=CurrIdx}|Des], HasAuto};
			 (#seat{seat_id=SeatIdx,mpid=Mpid,ext_s=ExtS}, {Des, HasAuto}) -> 
				  #zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,pass_deal=PassDeal,ban_deal=BanDeal} = ExtS,
				  Events = ai_mod:check_event(HandDeal, OpenDeal, PassDeal ++ BanDeal, DrawCard, ?DRAW_OTHER_EVENTS),
				  {[#de{mpid=Mpid,event_list=Events,seat_idx=SeatIdx,seat_idx_target=CurrIdx}|Des], HasAuto}
		  end,
	EventsWait = [De || #de{event_list=Events}=De <- element(?one, lists:foldr(Fun, {[], ?false}, Seats)), Events =/= []],
	Pred = fun(#de{event_list=Events}) -> lists:any(fun({Event, _}) -> ?CONST_ZPSYBP_CARD_STATE_WEI == Event end, Events) end,
	DrawMsg = desk_api:msg(?A_DESK_ZPSYBP_DISCARD, {CurrIdx, ?drawcard, [?IF(lists:any(Pred, EventsWait), #bpc{}, DrawCard)]}),
	DelaysNew = [{fun desk_api:broadcast/2, [self(), DrawMsg], ?one} | Delays],
	Desk#desk{event_wait=EventsWait,event_ing=?null,curr_seat_idx=CurrIdx,delay=DelaysNew}.


%%	出牌事件	
discard_event(Desk, CurrIdx, Discard) ->
	#desk{seat_list=Seats,lead_heap=Discarded} = Desk,
	ExtEvent = ?IF(length(Discarded) == ?one, [?CONST_ZPSYBP_CARD_STATE_WIN], []),
	NextIdx = ai_mod:next_seat_id_get(CurrIdx, Desk#desk.ext_d#zpsybp_d.player_limit),
	Fun = fun(#seat{seat_id=SeatIdx}, Des) when SeatIdx == CurrIdx ->
				  Des;
			 (#seat{seat_id=SeatIdx,mpid=Mpid,ext_s=ExtS}, Des) when SeatIdx == NextIdx ->
				  #zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,pass_deal=PassDeal,ban_deal=BanDeal} = ExtS,
				  Events = ai_mod:check_event(HandDeal, OpenDeal, PassDeal ++ BanDeal, Discard, ExtEvent ++ ?DISCARD_NEXT_EVENTS),
				  [#de{mpid=Mpid,event_list=Events,seat_idx=NextIdx,seat_idx_target=CurrIdx}|Des];
			 (#seat{seat_id=SeatIdx,mpid=Mpid,ext_s=ExtS}, Des) -> 
				  #zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,pass_deal=PassDeal,ban_deal=BanDeal} = ExtS,
				  Events = ai_mod:check_event(HandDeal, OpenDeal, PassDeal ++ BanDeal, Discard, ExtEvent ++ ?DISCARD_OTHER_EVENTS),
				  [#de{mpid=Mpid,event_list=Events,seat_idx=SeatIdx,seat_idx_target=CurrIdx}|Des]
		  end,
	EventsWait = [De || #de{event_list=Events}=De <- lists:foldr(Fun, [], Seats), Events =/= []],
	Desk#desk{event_wait=EventsWait,event_ing=?null,curr_seat_idx=CurrIdx}.



%%	处理事件
handle_event(Desk, Seat, {State, Deal}) ->
	#desk{curr_seat_idx=CurrIdx,event_finish=EventsFinish} = Desk,
	#seat{seat_id=SeatId} = Seat,
	?MSG_ECHO("~nHandleEvent::~p~n", [Deal]),
	De = #de{seat_idx=SeatId,seat_idx_target=CurrIdx,event_list=[{State, Deal}]},
	handle_event(Desk#desk{event_finish=[De|EventsFinish]}, Seat, State, Deal).

%%	出牌
handle_event(Desk, Seat, ?CONST_ZPSYBP_CARD_STATE_NORMAL, [Discard]) ->
	#desk{seat_list=Seats,lead_heap=Discarded,ext_d=ExtD} = Desk,
	#seat{seat_id=SeatId,mpid=Mpid,ext_s=ExtS} = Seat,
	#zpsybp_s{hand_deal=HandDeal,open_deal=OpenDeal,pass_deal=PassDeal} = ExtS,
	HandDealNew = ai_api:deep_rember([Discard], HandDeal),
	HuCurr = ai_api:profit_hide_find(HandDealNew) + ai_api:profit_curr_get(OpenDeal),
	PassDealNew = [Discard#bpc{state=State} || State <- ?PASS] ++ PassDeal,
	ExtSNew = ExtS#zpsybp_s{hu_curr=HuCurr,hand_deal=HandDealNew,pass_deal=PassDealNew},
	SeatNew = Seat#seat{ext_s=ExtSNew},
	SeatsNew = lists:keyreplace(SeatId, #seat.seat_id, Seats, SeatNew),
	ExtDNew = ExtD#zpsybp_d{focus_idx=SeatId,focus_card=Discard,focus_method=?discard},
	msg:send(Mpid, vipm_api:msg(?A_VIPM_ZPSYBP_INTO_EXT, SeatNew)),
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_ZPSYBP_DISCARD, {SeatId, ?discard, [Discard]})),
	discard_event(Desk#desk{seat_list=SeatsNew,lead_heap=[Discard|Discarded],state=?RECEIVING,state_time=?zero,ext_d=ExtDNew}, SeatId, Discard);
%%	过牌	
handle_event(Desk, Seat, ?CONST_ZPSYBP_CARD_STATE_CHOU, [Passcard]) ->
	#desk{seat_list=Seats,event_wait=DesWait,ext_d=ExtD,state_tag=StateLast} = Desk,
	#zpsybp_d{focus_method=Method} = ExtD,
	#seat{seat_id=SeatId,ext_s=ExtS} = Seat,
	#zpsybp_s{pass_deal=PassDeal} = ExtS,
	{?value, De, DeTail} = lists:keytake(SeatId, #de.seat_idx, DesWait),
	PassDealNew = [Passcard#bpc{state=Event} || {Event, _} <- De#de.event_list] ++ PassDeal,
	ExtSNew = ExtS#zpsybp_s{pass_deal=PassDealNew},
	SeatNew = Seat#seat{ext_s=ExtSNew},
	SeatsNew = lists:keyreplace(SeatId, #seat.seat_id, Seats, SeatNew),
	EventsNew = [{Event, Deal} || {Event, Deal} <- De#de.event_list, lists:member(Event, ?AUTO_EVENTS)],
	DesWaitNew = ?IF(?EMPTY_ARRAY(EventsNew), DeTail, [De#de{event_list=EventsNew} | DeTail]),
	ExtDNew = ExtD#zpsybp_d{focus_method=?IF(?EMPTY_ARRAY(EventsNew), Method, ?discard),hu_data=?null},
	DeskNew = Desk#desk{ext_d=ExtDNew,seat_list=SeatsNew,event_wait=DesWaitNew,state_tag=?null},
	case StateLast of
		?DISCARDING	-> 
			AltPointerMsg = desk_api:msg(?A_DESK_ZPSYBP_DISCARD, {SeatId, ?alt_pointer, []}),
			begin desk_api:broadcast(Desk, AltPointerMsg), DeskNew#desk{state=?DISCARDING} end;
		_Others		-> DeskNew
	end;
%%	偎牌/臭偎/提牌/偎提, 碰牌/吃牌/跑牌/碰跑/偎跑
handle_event(Desk, Seat, State, _Deal) ->
	#desk{event_ing=OldEventing,event_wait=DesWait,event_finish=[NewEventing|_],ext_d=#zpsybp_d{player_limit=Limit}} = Desk,
	{?value, De, DeTail} = lists:keytake(Seat#seat.seat_id, #de.seat_idx, DesWait),
	EventsNew = [{Event, Deal} || {Event, Deal} <- De#de.event_list, lists:member(Event, ?AUTO_EVENTS)],
	DesWaitNew = ?IF(?EMPTY_ARRAY(EventsNew) orelse lists:member(State, ?AUTO_EVENTS), DeTail, [De#de{event_list=EventsNew} | DeTail]),
	CurrEventing = ?IF(ai_mod:priority_event_check([OldEventing], NewEventing, Limit), NewEventing, OldEventing),
	receiving(Desk#desk{state_time=?one,event_wait=DesWaitNew,event_ing=CurrEventing}).

%%	游戏结束
game_over(Desk) ->
	#desk{vipm_key=RoomKey,banker_idx=BankerIdx,seat_list=Seats,time_end=Finish,card_heap=CartTail,bout_idx=BoutIdx,ext_d=ExtD,ext_dm=ExtDm} = Desk,
	{#zpsybp_d{hu_data={WinSeatId,HuData}}, #zpsybp_dm{last_banker_profit=Profit,satis_keep_banker=SatisKeep}} = {ExtD, ExtDm},
	IsDelay = lists:member(ai_mod:last_event_get(Desk), ?PAOS),
	[desk_api:broadcast(Desk, vipm_api:msg(?A_VIPM_ZPSYBP_INTO_EXT, Seat)) || Seat <- Seats],
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_ZPSYBP_VS_WAIT_END, {RoomKey, Finish, WinSeatId, HuData, IsDelay, CartTail, BankerIdx, Seats})),
	SatisBehavior = Profit < ?CONST_ZPSYBP_WIN_LIQUI_SATIS orelse WinSeatId == BankerIdx andalso HuData =/= ?barren,
	ai_mod:add_type_data_value(Seats, WinSeatId),
	case lists:all(fun(Acc) -> Acc < ?CONST_ZPSYBP_WIN_LIQUI_SATIS end, [Profit | [ExtS#zpsybp_s.hu_acc || #seat{ext_s=ExtS} <- Seats]]) of
		Satis when Satis; SatisKeep, SatisBehavior	->			
			SoList = ai_mod:make_so_list(Seats),
			dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_update, [RoomKey, ?one, BoutIdx, SoList]),
			Desk#desk{banker_idx=WinSeatId,time_end=Finish,state=?GAME_PREP};
		?false	->
			Desk#desk{banker_idx=WinSeatId,time_end=Finish,state=?DISBAND_ROOM} 
	end.
