%% @author Administrator
%% @doc ai_sj_mod 用于处理在 desk_sj_mod 中没有实现的具体定义.
-module(sj_ai_mod).
%% TODO 不要让 get_point 到处乱飞
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("/scene.hrl").
-include("/sj.hrl").

-export([
		 liq_score/3,
		 up_level/6,
		 set_stand_for/2,
		 get_stand_for/2,
		 get_profit/3,
		 
		
		 prompt/4,
		 leading/3,
		 seats_renew/2,
		 pick_banker/3,
		 
		 make_so_data/1,
		 make_desk_msg/1,
		 make_seat_msg/2,
		 make_vote_msg/2,
		 make_open_msg/3,
		 		 
		 send_vslogs_msg/1,
		 save_vslogs_db/1
		]).

%% --------------------------------------------------------------------
%% internal functions
%% --------------------------------------------------------------------

%%	设置总结算玩家分数
liq_score(?bout, _Multiple, Seats) ->
	Seats;
liq_score(?level, Multiple, Seats) ->
	case Multiple of
		0	->
			Score = ?one;
		Multiple	->
			Levels = [SjS#sj_s.level || #seat{ext_s=SjS} <- Seats],
			LevelDiff = lists:max(Levels) - lists:min(Levels),
			Score = LevelDiff * Multiple
	end,
	Eval = fun
			  (#sj_s{stand_for=?defender}=SjS) ->
				   SjS#sj_s{score=+Score};
			  (#sj_s{stand_for=?attacker}=SjS) -> 
				   SjS#sj_s{score=-Score} 
		   end,
	[Seat#seat{ext_s=Eval(Seat#seat.ext_s)} || Seat <- Seats].


%%	根据分数升级,计算金币、积分
up_level(Mode, Seats, Winner, Score, Base, IsUplevel) ->
	UpSocre = sj_ai_api:eval_rate(Score) * Base,
	UpLevel = ?IF(IsUplevel, sj_ai_api:eval_level(Score), 0),
	Eval = fun
			  (#sj_s{score=Score,win_times=Wtimes,level=Level,stand_for=Stf}=SjS) when Stf == Winner ->
				   begin MaybeLevel = Level + UpLevel, OverLevel = MaybeLevel rem 13 end,
				   ActualLevel = ?IF(Mode == ?level andalso MaybeLevel > ?ace, ?ace, ?IF(OverLevel < 2, OverLevel + 13, OverLevel)),
				   SjS#sj_s{score=Score+UpSocre,offset_score=+UpSocre,offset_level=UpLevel,level=ActualLevel,win_times=Wtimes+?one};
			  (#sj_s{score=Score}=SjS) -> 
				   SjS#sj_s{score=Score-UpSocre,offset_score=-UpSocre,offset_level=0} 
		   end,
	{UpSocre, UpLevel, [Seat#seat{ext_s=Eval(Seat#seat.ext_s)} || Seat <- Seats]}.


%%	设置立场
set_stand_for(Seats, BankerUid) ->
	#seat{seat_id=BankerIdx} = lists:keyfind(BankerUid, #seat.uid, Seats),
	GetStf = fun(SeatId) -> ?IF(BankerIdx rem ?two == SeatId rem ?two, ?defender, ?attacker) end,
 	[Seat#seat{ext_s=SjS#sj_s{stand_for=GetStf(Idx)}} || #seat{seat_id=Idx,ext_s=SjS}=Seat <- Seats].

get_stand_for(Seats, Uid) ->
	(lists:keyfind(Uid, #seat.uid, Seats))#seat.ext_s#sj_s.stand_for.


%%	获取分数
get_profit(Seats, Muid, Cards) ->
	case get_stand_for(Seats, Muid) of
		?attacker	-> sj_ai_api:get_profit(Cards);
		?defender	-> 0
	end.
	

%%	第一次出牌
leading(#desk{event_ing=?null}=Desk, Seat, Cards) ->
	#desk{seat_list=Seats,ext_d=SjD,delay=Delays} = Desk,
	#sj_d{trump_suit=Ts,trump_point=Tp} = SjD,
	#seat{seat_id=SeatId,uid=Uid,mpid=Mpid,ext_s=SjS} = Seat,
	#sjc{suit=Suit,point=Point} = hd(Cards),
	NextUid = next_uid_get(SeatId, Seats),
	Rs = ?IF(Point == Tp orelse Point > ?ace, Ts, Suit),
	case sj_ai_api:eq_suit(Ts, Tp, Rs, Cards) of		
		?false	-> 
			begin system_api:send_error(Mpid, ?E_GAME_ILLEGAL_PLAY), Desk end;
		?true	->
			Points = sj_ai_api:get_point(Ts, Tp, Cards),
			HcsList = [Hcs || #seat{ext_s=#sj_s{hand_cards=Hcs}} <- lists:keydelete(Uid, #seat.uid, Seats)],
			PsList = [[sj_ai_api:get_point(Ts, Tp, C) || C <- Hcs, sj_ai_api:eq_suit(Ts, Tp, Rs, C)] || Hcs <- HcsList],
			MakeMsg = fun(Type, IsSucc, Cs) -> sj_desk_api:msg(?A_DESK_SJ_LEADING, {Uid, IsSucc, ?true, Type, ?justlead, 0, Cs}) end,
			PointerMsg = sj_desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {NextUid, ?alt_pointer, []}),
			case {sj_ai_api:get_type(Points), sj_ai_api:throw(Points, PsList)} of
				{?CONST_SJ_XW_THROW, Rcards} when Rcards /= ?true	->	% 甩牌失败
					system_api:send_error(Mpid, ?E_ADD_ERROR25),
					RealRcards = [Card#sjc{ref_point=0} || Card <- Rcards],
					desk_api:broadcast(Desk, MakeMsg(?CONST_SJ_XW_THROW, ?false, Cards)),
					TmpMsg = MakeMsg(sj_ai_api:get_type(RealRcards), ?true, RealRcards),
					Delay = {fun desk_api:broadcast/2, [self(), <<TmpMsg/?binary, PointerMsg/?binary>>], ?three},
					lead(Desk#desk{curr_uid=NextUid,event_ing=#de{seat_idx=Uid},delay=[Delay|Delays]}, Seat, RealRcards, ?true, ?justlead);
				{Type, _Val} ->
					desk_api:broadcast(Desk, <<(MakeMsg(Type, ?true, Cards))/?binary, PointerMsg/?binary>>),
					lead(Desk#desk{curr_uid=NextUid,event_ing=#de{seat_idx=Uid}}, Seat, Cards, ?true, ?justlead)
			end
	end;
%%	其他人出牌
leading(Desk, Seat, Cards) ->
	#desk{seat_list=Seats,event_ing=Eing,ext_d=SjD} = Desk,
	{#sj_d{trump_suit=Ts,trump_point=Tp}, #de{event_list=Elist,event_finish={Muid,Mcards}}} = {SjD, Eing},
	#seat{mpid=Mpid,seat_id=SeatId,uid=Uid,ext_s=SjS} = Seat,
	#sj_s{defeat_times=Dtimes} = SjS,
	Rcards = element(?three, hd(Elist)),
	NextUid = next_uid_get(SeatId, Seats),	
	Points = sj_ai_api:get_point(Ts, Tp, Cards),
	Type = sj_ai_api:get_type(Points),
	PointerMsg = ?IF(length(Elist) < ?three, sj_desk_api:msg(?A_DESK_SJ_CARDS_EVENT, {NextUid, ?alt_pointer, []}), <<>>),
	MakeArgs = fun(MaxUid, Action) -> {Uid, ?true, ?false, Type, Action, MaxUid, Cards} end,
	Broadcast = fun(MsgId, Args) -> desk_api:broadcast(Desk, <<(sj_desk_api:msg(MsgId, Args))/?binary, PointerMsg/?binary>>) end,
	case check_valid(Seat, Ts, Tp, Rcards, Cards) of
		invalid		-> 
			begin system_api:send_error(Mpid, ?E_GAME_ILLEGAL_PLAY), Desk end;
		justlead	->
			Broadcast(?A_DESK_SJ_LEADING, MakeArgs(Muid, ?justlead)),
			lead(Desk#desk{curr_uid=NextUid}, Seat, Cards, ?false, ?justlead);
		compare		->
			Rps = sj_ai_api:get_point(Ts, Tp, Rcards),
			MaxPs = sj_ai_api:get_point(Ts, Tp, Mcards),
			IsBiggest = sj_ai_api:lead(Points, MaxPs, Rps) /= ?true,
			Action = sj_ai_api:lead_action(Ts, Tp, Cards, Rcards, Mcards, IsBiggest),
			DtimesNew = ?IF(Action /= ?justlead, Dtimes + ?one, Dtimes),
			Broadcast(?A_DESK_SJ_LEADING, MakeArgs(?IF(IsBiggest, Uid, Muid),Action)),
			lead(Desk#desk{curr_uid=NextUid}, Seat#seat{ext_s=SjS#sj_s{defeat_times=DtimesNew}}, Cards, IsBiggest, Action)
	end.

%%	出牌
lead(Desk, Seat, Cards, IsBiggest, Action) ->
	#desk{event_ing=Eing,seat_list=Seats,ext_d=SjD} = Desk,
	{#sj_d{trump_suit=Ts,trump_point=Tp}, #de{event_list=Elist,event_finish=Emax}} = {SjD, Eing},
	#seat{uid=Uid,ext_s=#sj_s{hand_cards=HandCards}=SjS} = Seat,
	#sj_s{tractor_times=Ttimes} = SjS,
	EmaxNew = ?IF(IsBiggest, {Uid, Cards}, Emax),
	EingNew = Eing#de{event_list=Elist++[{Action, Uid, Cards}],event_finish=EmaxNew},
	TtimesNew = sj_ai_api:get_tractor_times(Ts, Tp, element(?three, lists:unzip3(Elist)), Cards) + Ttimes,
	SeatNew = Seat#seat{ext_s=SjS#sj_s{hand_cards=HandCards--Cards,tractor_times=TtimesNew}},
	SeatsNew = lists:keyreplace(Uid, #seat.uid, Seats, SeatNew),
	Desk#desk{seat_list=SeatsNew,event_ing=EingNew}.


%%	检查出牌合法性
check_valid(Seat, Ts, Tp, Rcards, Cards) when length(Rcards) == length(Cards) ->
	#seat{ext_s=#sj_s{hand_cards=Hcs}} = Seat,
	[#sjc{suit=Suit,point=Point} | _] = Rcards,
	Rs = ?IF(Point == Tp orelse Point > ?ace, Ts, Suit),
	Psg = sj_ai_api:group(sj_ai_api:get_point(Ts, Tp, Cards)),
	Rpsg = sj_ai_api:group(sj_ai_api:get_point(Ts, Tp, Rcards)),
	Hpsg = sj_ai_api:group([sj_ai_api:get_point(Ts, Tp, C) || C <- Hcs, sj_ai_api:eq_suit(Ts, Tp, Rs, C)]),
	case sj_ai_api:eq_suit(Ts, Tp, Rs, Cards) of
		?false	-> 
			case [C || C <- Hcs, sj_ai_api:eq_suit(Ts, Tp, Rs, C)] -- Cards of
				[]	->
					case sj_ai_api:eq_suit(Ts, Tp, Ts, Cards) of
						?true	-> ?IF(sj_ai_api:check_same_type(Rpsg, Psg), compare, justlead);
						?false	-> justlead
					end;
				_	-> 
					invalid
			end;
		?true	->
			case sj_ai_api:check_same_type(Rpsg, Psg) of
				?true	-> compare;
				?false	-> ?IF(sj_ai_api:check_have_unlead(Hpsg, Rpsg, Psg), invalid, justlead)
			end
	end;
check_valid(_Seat, _Ts, _Tp, _Rcards, _Cards) ->
	invalid.


%%	出牌提示
prompt(Ts, Tp, WholeHcs, Rcs) ->
	#sjc{suit=Suit,point=Point} = hd(Rcs),
	Rs = ?IF(Point == Tp orelse Point > ?ace, Ts, Suit),
	Hcs = [C || C <- WholeHcs, sj_ai_api:eq_suit(Ts, Tp, Rs, C)],
 	Hps = [sj_ai_api:get_point(Ts, Tp, C) || C <- Hcs],	
	Rps = [sj_ai_api:get_point(Ts, Tp, C) || C <- Rcs],
	Hpsg = [lists:reverse(Psl) || Psl <- sj_ai_api:group(Hps)],
	Rpsg = sj_ai_api:group(Rps),
	case length(Hps) > length(Rps) of
		?true	-> lists:flatten(sj_ai_api:find_occur(Hpsg, Rpsg));
		?false	-> []
	end.

%% --------------------------------------------------------------------
%% free functions
%% --------------------------------------------------------------------

%%	生成一个座位
seats_renew(Seats, SeatTmp) ->
	#seat{uid=Uid,uname=Uname,skin_id=SkinId,icon_id=IconId,icon_url=IconUrl,vip_lv=VipLv,sex=Sex,lv=Lv,charm=Charm,mpid=Mpid,socket=Socket} = SeatTmp,
	case lists:keytake(Uid, #seat.uid, Seats) of
		?false	->
			SeatId = idle_seat_id_get([Id || #seat{seat_id=Id} <- Seats]),
			[SeatTmp#seat{seat_id=SeatId} | Seats];
		{?value, SeatOld, SeatTail}	->
			SeatNew = SeatOld#seat{uname=Uname,skin_id=SkinId,icon_id=IconId,icon_url=IconUrl,vip_lv=VipLv,sex=Sex,lv=Lv,charm=Charm,mpid=Mpid,socket=Socket},
			[SeatNew | SeatTail]
	end.


%%	获取空闲座位id
idle_seat_id_get(OldIds) ->
	Ids = lists:seq(1, ?CONST_SJ_GAME_STATE_NUM_PLAYER) -- OldIds,
	?IF(?EMPTY_ARRAY(Ids), 0, hd(Ids)).


%%	获取下家的uid
next_uid_get(SeatId, Seats) when SeatId =< length(Seats) ->
	NextSeatId = ?IF(SeatId == length(Seats), ?one, SeatId + ?one),
	(lists:keyfind(NextSeatId, #seat.seat_id, Seats))#seat.uid;
next_uid_get(Uid, Seats) ->
	#seat{seat_id=SeatId} = lists:keyfind(Uid, #seat.uid, Seats),
	next_uid_get(SeatId, Seats).

pick_banker(?attacker, BankerUid, Seats) ->
	next_uid_get(BankerUid, Seats);
pick_banker(?defender, BankerUid, Seats) ->
	next_uid_get(next_uid_get(BankerUid, Seats), Seats).
	
%% --------------------------------------------------------------------
%% msg functions
%% --------------------------------------------------------------------
%%	生成房间拓展数据
make_so_data([Seat|Seats]) ->
	#seat{uid=Uid,uname=Uname,icon_url=IconUrl,ext_s=#sj_s{level=Level}} = Seat,
	[#so{uid=Uid,uname=Uname,icon_url=IconUrl,chip=Level} | make_so_data(Seats)];
make_so_data([]) ->
	[].

%%	生成桌子信息
make_desk_msg(Desk) ->
	#desk{consume_card=CardNum,master_uid=MasterUid,bout_total=Btotal,bout_idx=Bidx,ext_dm=SjDm} = Desk,
	#sj_dm{game_mode=Mode,multiple=Mutiple,default_level=DefLevel,is_uplevel=IsLevelup} = SjDm,
	Data = ?IF(Mode == ?one, Btotal, Mutiple),
	Ext = {CardNum, Mode, DefLevel, Data, IsLevelup},
	vipm_api:msg_update(Desk, Btotal, Bidx, MasterUid, Ext).

%%	生成座位信息
make_seat_msg(Desk, Seat) ->
	vipm_api:msg_into(Desk, Seat, Seat).

%% 生成投票信息
make_vote_msg(#vote_info{creator_uid=VoterUid,end_time=End,v_list=VList}, Uid) when VoterUid =/= 0 ->
	State = lists:keymember(?one, Uid, VList),
	Left = End - util_time:seconds(),
	Bin1 = msg:encode([{?bool, ?true}]),
	Bin2 = vipm_api:msg_group(?A_VIPM_VOTE, {VoterUid, Left, State, VList}),
	<<Bin1/binary, Bin2/binary>>;
make_vote_msg(_Else, _Uid) ->
	msg:encode([{?bool, ?false}]).

%% 生成出牌信息
make_open_msg(Ts, Tp, #de{seat_idx=Fuid,event_list=Elist,event_finish={Muid,_Mcards}}) ->
	GetType = fun(Cs) -> sj_ai_api:get_type(sj_ai_api:get_point(Ts, Tp, Cs)) end,
	MakeArgs = fun({Act, Uid, Cs}) -> {Uid, ?true, Uid == Fuid, GetType(Cs), Act, Muid, Cs} end,
	Bin1 = msg:encode([{?int8u, length(Elist)}]),
	Bin2 = iolist_to_binary([sj_desk_api:msg_group(?A_DESK_SJ_LEADING, MakeArgs(Stuff)) || Stuff <- Elist]),
	<<Bin1/?binary, Bin2/?binary>>;
make_open_msg(_Ts, _Tp, ?null) ->
	<<0>>.

%%	发送对战日志信息
send_vslogs_msg(Desk) ->
	#desk{game_id=GameId,desk_id=DeskId,vipm_key=RoomKey,room_type=RoomType,bout_idx=BoutIdx,seat_list=Seats,time_start=Begin,time_end=Finish} = Desk,
	Mode = Desk#desk.ext_dm#sj_dm.game_mode,
	MakeOffset = fun(SjS) -> ?IF(Mode == ?bout, SjS#sj_s.offset_score, SjS#sj_s.offset_level) end,
	VsLogs = [{BoutIdx, Begin, [{Uid, [{?CONST_VGOODS_SCORE, MakeOffset(SjS)}]} || #seat{uid=Uid,ext_s=SjS} <- Seats]}],
	[msg:send(Mpid, vipm_api:msg(?A_VIPM_LOGS_INFO, {GameId, DeskId, RoomKey, Finish, RoomType, Score, VsLogs})) || #seat{mpid=Mpid,ext_s=#sj_s{score=Score}} <- Seats].

%%	保存对战日志到数据库
save_vslogs_db(Desk) ->
	#desk{seat_list=Seats,ext_dm=#sj_dm{game_mode=Mode}} = Desk,
	MakeOffset = fun(SjS) -> ?IF(Mode == ?bout, SjS#sj_s.offset_score, SjS#sj_s.offset_level) end,
	PublicDatas = [{Uid, [{?CONST_VGOODS_SCORE, MakeOffset(SjS)}]} || #seat{uid=Uid,ext_s=SjS} <- Seats],
	vipm_api:logs_ka_about(Desk, PublicDatas, [], 0, [], <<>>). 
