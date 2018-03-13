%% -------------------------------------------------------------------
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
-include("mod.game_sj.hrl").
-include("mod.desk_scene.hrl").  

-export([
		 game_on/1,
		 eventing/4,
		 game_over/1,
		 round_over/1,
		 
		 deal_card/1,
		 bury_card/1,
		 flip_card/1
		]).
%% --------------------------------------------------------------------
%% game function
%% --------------------------------------------------------------------

%%	游戏开始
game_on(Desk) ->
	#desk{vipm_key=RoomKey,seat_list=Seats,banker_idx=BankerUid,bout_idx=BoutIdx,bout_total=BoutTotal,state=State} = Desk,
	#sj_d{trump_point=Tp} = Desk#desk.ext_d,
	SoList = ai_mod:make_so_data(Seats), 	
	dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_update, [RoomKey, ?one, BoutIdx, SoList]),
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_SJ_GAME_DATA, {State, BoutIdx, BoutTotal, BankerUid, Tp, Seats})),
	NewSjD = Desk#desk.ext_d#sj_d{winner=?zero,profit=?zero,lord_data={?zero, []},trump_suit=?trump},	
	Desk#desk{ext_d=NewSjD,curr_uid=?zero,seat_list=Seats,card_heap=[],event_finish=[],event_ing=?null}.


%%	游戏结束
game_over(Desk) ->
	#desk{vipm_key=RoomKey,time_end=Finish,bout_idx=Bidx,bout_total=Btotal,ext_d=SjD} = Desk,
	DeskNew = #desk{banker_idx=BankerUid,seat_list=Seats} = flip_card(Desk),
	Blevel = (lists:keyfind(BankerUid, #seat.uid, Seats))#seat.ext_s#sj_s.level,
	SjDNew = SjD#sj_d{trump_point=Blevel},
	case Desk#desk.ext_dm#sj_dm.game_mode of
		Mode when Mode == ?bout andalso Bidx >= Btotal; Mode == ?level andalso Blevel >= ?ace	->
%% 		_	->
			GameState = ?GAME_DISBAND;
		_Otherwise	->
			GameState = ?GAME_PREP,
			SoList = ai_mod:make_so_data(Seats),
			dcfg:rpc_cast_hub(vipm_ctrl_api, vipm_update, [RoomKey, ?one, Bidx, SoList])
	end,
	DeskNew#desk{ext_d=SjDNew,time_end=Finish,state=GameState}.					


%%	回合结束
round_over(Desk) ->
	#desk{seat_list=Seats,event_finish=Efinish,event_ing=Eing,ext_d=SjD,delay=Delays} = Desk,
	{#sj_d{profit=Opacc}, #de{event_list=Elist,event_finish={Muid,_Mcards}}} = {SjD, Eing},
	PointerMsg = desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {Muid, ?alt_pointer, []}),
	DelaysNew = [{fun desk_api:broadcast/2, [self(), PointerMsg], ?two} | Delays],
	Pcurr = ai_mod:get_profit(Seats, Muid, element(?three, lists:unzip3(Elist))),
	Pacc = Opacc + Pcurr,
	SjDNew = SjD#sj_d{profit=Pacc},
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_SJ_SCORE_DATA, {Pacc, Pcurr})),
	GameState = ?IF((hd(Seats))#seat.ext_s#sj_s.hand_cards == [], ?GAME_OVER, ?GAME_LEAD),
	Desk#desk{curr_uid=Muid,state=GameState,delay=DelaysNew,event_ing=?null,event_finish=[Eing|Efinish],ext_d=SjDNew}.
	

%%	发牌
deal_card(Desk) ->
	#desk{seat_list=Seats,state_time=StateTime} = Desk,
	{CardsList, CardHeap} = lists:split(?CONST_SJ_GAME_STATE_NUM_PLAYER, ai_api:dealing2()),
	Iter = fun({#seat{uid=Uid,mpid=Mpid,ext_s=SjS}=Seat, Cards}, SeatsNew) ->
				   msg:send(Mpid, desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {Uid, ?dealing, Cards})),
				   [Seat#seat{is_game=?true,is_ready=?false,ext_s=SjS#sj_s{hand_cards=Cards}} | SeatsNew]
		   end,
	SeatsNew = lists:foldr(Iter, [], lists:zip(Seats, lists:sublist(CardsList, length(Seats)))),
	Desk#desk{seat_list=SeatsNew,card_heap=CardHeap,state_time=StateTime+?one}.


%%	亮主
lord_card(Desk, Uid, Cards, Behavior) ->
	#desk{seat_list=Seats,ext_d=SjD} = Desk,
	{?value, #seat{ext_s=#sj_s{lord_times=Ltimes}=SjS}=Seat, Tail} = lists:keytake(Uid, #seat.uid, Seats),
	SeatsNew = [Seat#seat{ext_s=SjS#sj_s{lord_times=Ltimes+?one}} | Tail],	
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {Uid, Behavior, Cards})),
	Desk#desk{seat_list=SeatsNew,ext_d=SjD#sj_d{lord_data={Uid,Cards},trump_suit=(hd(Cards))#sjc.suit}}.


%%	扣底
bury_card(#desk{banker_idx=?zero}=Desk) ->
	case Desk#desk.ext_d#sj_d.lord_data of
		{?zero, []}	->
			desk_api:broadcast_error(Desk, ?E_ADD_ERROR26), 
			Desk#desk{state_time=?zero};
		{Uid, _Cards}	->
			Desk#desk{banker_idx=Uid}
	end;
bury_card(Desk) ->
	#desk{state=State,seat_list=Seats,banker_idx=BankerUid,card_heap=CardHeap,bout_idx=BoutIdx,bout_total=BoutTotal,ext_d=SjD} = Desk,
	#sj_d{trump_point=Tp} = SjD,
	SeatsNew = ai_mod:set_stand_for(Seats, BankerUid),
	msg:send(BankerUid, desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {BankerUid, ?burying, CardHeap})),
	BuryMsg = desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {BankerUid, ?burying, []}),
	GameMsg = desk_api:msg(?A_DESK_SJ_GAME_DATA, {State, BoutIdx, BoutTotal, BankerUid, Tp, SeatsNew}),
	desk_api:broadcast(Desk, <<BuryMsg/?binary, GameMsg/?binary>>),
	{?value, #seat{ext_s=SjS}=Banker, Tail} = lists:keytake(BankerUid, #seat.uid, SeatsNew),
	BankerNew = Banker#seat{ext_s=SjS#sj_s{hand_cards=SjS#sj_s.hand_cards++CardHeap}},
	Desk#desk{curr_uid=BankerUid,seat_list=[BankerNew|Tail],card_heap=[],state=?GAME_LEAD,state_time=?zero}.


%%	抄底
flip_card(Desk) ->
	#desk{seat_list=Seats,event_finish=[Eing|_Efinish],banker_idx=BankerUid,card_heap=Heap,state=State,bout_idx=BoutIdx,bout_total=BoutTotal,ext_d=SjD,ext_dm=SjDm} = Desk,
	{#sj_d{trump_suit=Ts,trump_point=Tp,profit=Profit}, #sj_dm{game_mode=Mode,is_uplevel=IsUplevel}, #de{event_finish={Muid,Mcards}}} = {SjD, SjDm, Eing},
	Flipper = ai_mod:get_stand_for(Seats, Muid),
	Mps = [ai_api:get_point(Ts, Tp, C) || C <- Mcards],
	Ptotal = ai_api:eval_flip(Flipper, Mps, Heap) + Profit,
	Winner = ?IF(Ptotal >= 80, ?attacker, ?defender),
	NewBankerUid = ai_mod:pick_banker(Winner, BankerUid, Seats),
	{_UpScore, UpLevel, SeatsNew} = ai_mod:up_level(Mode, Seats, Winner, Ptotal, ?one, IsUplevel),
	FlipMsg = desk_api:msg(?A_DESK_SJ_FLIPING, {Flipper, Heap, {Ptotal, Ptotal - Profit}}),
	GameMsg = desk_api:msg(?A_DESK_SJ_GAME_DATA, {State, BoutIdx, BoutTotal, BankerUid, Tp, SeatsNew}),
	SettleMsg = desk_api:msg(?A_DESK_SJ_SETTLEMENT, {Ptotal, Winner, UpLevel, BankerUid, SeatsNew}),
	desk_api:broadcast(Desk, <<FlipMsg/?binary, GameMsg/?binary, SettleMsg/?binary>>),
	Desk#desk{banker_idx=NewBankerUid,seat_list=SeatsNew,ext_d=SjD#sj_d{winner=Winner,profit=Ptotal}}.	
	

%%	亮主事件
eventing(Desk, ?lording, Seat, Cards) ->
	#desk{ext_d=#sj_d{lord_data=LordData,trump_suit=Ts,trump_point=Tp}} = Desk,
	{#sjc{suit=Ls,point=Lp}, Ln, Uid} = {hd(Cards), length(Cards), Seat#seat.uid},
	case LordData of
		{?zero, []}	->
			lord_card(Desk, Uid, Cards, ?justlord);
		{Uid, Lcards} when length(Lcards) == ?one, Ls == Ts	->
			lord_card(Desk, Uid, Cards, ?defended);
		{Luid, Lcards} when Luid /= Uid, Lp > Tp orelse Ln > length(Lcards)	->
			lord_card(Desk, Uid, Cards, ?rushlord);
		{_Uid, _Cards}	->
			Desk
	end;
%%	扣底事件
eventing(Desk, ?burying, Banker, Cards) ->
	{#desk{seat_list=Seats}, #seat{uid=BankerUid,ext_s=SjS}} = {Desk, Banker},
	desk_api:broadcast(Desk, desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {BankerUid, ?alt_pointer, []})),
	BankerNew = Banker#seat{ext_s=SjS#sj_s{hand_cards=SjS#sj_s.hand_cards--Cards}},
	SeatsNew = lists:keyreplace(BankerUid, #seat.uid, Seats, BankerNew),
	Desk#desk{curr_uid=BankerUid,card_heap=Cards,seat_list=SeatsNew};
%%	出牌事件 
eventing(Desk, ?leading, Seat, Cards) ->
	?MSG_COUT("~nLead Cards::~w", [Cards], "\e[1;7;32m"),
	ai_mod:leading(Desk, Seat, Cards);
%%	提示出牌
eventing(Desk, ?prompt, Seat, _Cards) ->
	#desk{event_ing=Eventing,ext_d=#sj_d{trump_suit=Ts,trump_point=Tp}} = Desk,
	#seat{uid=Uid,mpid=Mpid,ext_s=#sj_s{hand_cards=Hcards}} = Seat,
	case Eventing of
		#de{event_list=[{_Raction, _Ruid, Rcards}|_]}	->
			Prompt = ai_mod:prompt(Ts, Tp, Hcards, Rcards),
			msg:send(Mpid, desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {Uid, ?prompt, Prompt})),
			Desk;
		_Otherwise	-> 
			Desk
	end.
