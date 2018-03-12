-module(sj_ai_api).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("/scene.hrl").
-include("/sj.hrl").  

-type deals() :: [deal()].
-type deal() :: [card()].
-type card() :: tuple().

-export([
		 deck_get/0,
		 dealing/0,
		 dealing2/0,
		 
		 eq_suit/4,
		 
		 check_same_type/2,
		 check_have_unlead/3,

		 lead/3,
		 throw/2,
		 
		 get_point/3,
		 get_type/1,
		 get_profit/1,
		 get_tractor_times/4,
		 lead_action/6,
		 
		 
		 find_lack/2,
		 find_occur/2,
		  
		 group/1
		]).

-export([
		 eval_flip/3,
		 eval_rate/1,
		 eval_level/1
		]).

%% ====================================================================
%% Internal functions
%% ====================================================================

%%	造两副牌
-spec deck_get() -> Deck when
      Deck :: [card()].

deck_get() -> 
	Cards = [#sjc{suit=S,point=P} || S <- ?suits, P <- ?points],
	Jokers = [#sjc{suit=?trump,point=P} || P <- ?joker_point],
	lists:flatten(lists:duplicate(?two, Cards ++ Jokers)).


%%	发牌
-spec dealing() -> DealHeap when
      DealHeap :: deals().

dealing() ->
	CardHeap = util:lists_shuffle(deck_get()),
	dealing(?CONST_SJ_GAME_STATE_NUM_PLAYER, CardHeap).

dealing(?zero, Heap) ->
	Heap;
dealing(Num, Heap) ->
	{Cards, HeapTail} = lists:split(?CONST_SJ_GAME_STATE_NUM_CARD, Heap),
	[Cards | dealing(Num - 1, HeapTail)].


%%	邪恶的造牌术
dealing2() ->
	CardHeap = util:lists_shuffle(deck_get()),
	Cs1 = lists:duplicate(2, [#sjc{suit=1,point=P} || P <- lists:seq(11, 14)]),
	Cs2 = lists:duplicate(2, [#sjc{suit=1,point=3},#sjc{suit=1,point=4},#sjc{suit=1,point=7},#sjc{suit=1,point=9}]) ++ [#sjc{suit=1,point=5},#sjc{suit=1,point=6}],
	CsHeap = [lists:flatten(Cs1), lists:flatten(Cs2)],
	dealing2(?CONST_SJ_GAME_STATE_NUM_PLAYER, CsHeap, CardHeap -- lists:flatten(CsHeap)).

dealing2(Num, [], Heap) ->
	dealing(Num, Heap);
dealing2(Num, [Cs | CsHeap], Heap) ->
	Len = ?CONST_SJ_GAME_STATE_NUM_CARD - length(Cs),
	{Cards, HeapTail} = lists:split(Len, Heap),
	[Cards ++ Cs | dealing2(Num - 1, CsHeap, HeapTail)].


%%	是否相同花色
-spec eq_suit(Ts, Tp, Rs, Deal) -> boolean() when
      Ts :: byte(),
      Tp :: byte(),
      Rs :: byte(),
      Deal :: deal().

eq_suit(Ts, Tp, Rs, Cards) when is_list(Cards) ->	
	lists:all(fun(Card) -> eq_suit(Ts, Tp, Rs, Card) end, Cards);
eq_suit(S, _Tp, S, #sjc{suit=S}) ->
	?true;
eq_suit(_Ts, Tp, S, #sjc{suit=S,point=P}) ->
	Tp /= P;
eq_suit(S, Tp, S, #sjc{suit=Suit,point=P}) ->
	Tp == P orelse Suit == ?trump;
eq_suit(_Ts, _Tp, _Rs, _Card) ->
	?false.


%%	检查牌型是否相同
-spec check_same_type(PsgA, PsgB) -> boolean() when
      PsgA :: [deals(),...],
      PsgB :: [deals()].

check_same_type(PsgA, PsgB) ->
	find_lack(PsgB, PsgA) == [].


%%	判断手牌中是否有未出的牌型
-spec check_have_unlead(Hpsg, Rpsg, Psg) -> boolean() when
      Hpsg :: [deals()],
      Rpsg :: [deals(),...],
      Psg :: [deals(),...].

check_have_unlead(Hpsg, Rpsg, Psg) ->	
	case find_lack(Hpsg, Rpsg) == find_lack(Psg, Rpsg) of
		?false	->
			?true;
		?true	->
			case lists:splitwith(fun(E) -> length(hd(E)) > ?two end, Rpsg) of
				{[], _}	->
					?false;
				{Tractors, TlRePsg}	->
					{Couples, Tails} = lists:unzip([lists:split(?two, T) || T <- lists:merge(Tractors)]),
					check_have_unlead(Hpsg, gappend(gappend(TlRePsg, Couples), Tails), Psg)
			end
	end.


%%	推导出所有缺少的牌型
-spec find_lack(PsgA, PsgB) -> PsgLack when
      PsgA :: [deals(),...],
      PsgB :: [deals(),...],
      PsgLack :: [deals()].
		  
find_lack([PslA | PsgA], [PslB | PsgB]) when length(hd(PslA)) == length(hd(PslB)) ->
	{PslLenA, PslLenB} = {length(PslA), length(PslB)},
	if
		PslLenA == PslLenB	->
			find_lack(PsgA, PsgB);
		PslLenA > PslLenB	->		
			find_lack([lists:nthtail(PslLenB, PslA) | PsgA], PsgB);
		PslLenA < PslLenB	->		
			[lists:nthtail(PslLenA, PslB) | find_lack(PsgA, PsgB)]
	end;
find_lack([PslA | PsgA], [PslB | PsgB]) when length(hd(PslA)) > length(hd(PslB)) -> 
	{Match, Overflow} = lists:unzip([lists:split(length(hd(PslB)), Ps) || Ps <- PslA]),
	find_lack(gappend(gappend(PsgA, Match), Overflow), [PslB | PsgB]);
find_lack([PslA | PsgA], [PslB | PsgB]) when length(hd(PslA)) < length(hd(PslB)) ->
	[PslB | find_lack([PslA | PsgA], PsgB)];
find_lack(_PsgA, []) ->	
	[].


%%	推导出所有共有的牌型
-spec find_occur(PsgA, PsgB) -> PsgOccur when
      PsgA :: [deals()],
      PsgB :: [deals(),...],
      PsgOccur :: [deals()].

find_occur(_PsgA, PsgB) when PsgB == []; length(hd(hd(PsgB))) < ?two ->
	[];
find_occur([PslA | PsgA], [PslB | PsgB]) when length(hd(PslA)) == length(hd(PslB)) ->
	{PslLenA, PslLenB} = {length(PslA), length(PslB)},
	if
		PslLenA == PslLenB	->
			[PslA | find_occur(PsgA, PsgB)];
		PslLenA > PslLenB	->
			{Must, Maybe} = lists:split(PslLenB, PslA),
			[Must | find_occur(gappend(PsgA, Maybe), PsgB)];
		PslLenA < PslLenB	->
			[PslA | find_occur(PsgA, [lists:nthtail(PslLenA, PslB) | PsgB])]
	end;
find_occur([PslA | PsgA], [PslB | PsgB]) when length(hd(PslA)) > length(hd(PslB)) ->
	{Match, Overflow} = lists:unzip([lists:split(length(hd(PslB)), Ps) || Ps <- PslA]),
	find_occur(gappend(gappend(PsgA, Match), Overflow), [PslB | PsgB]);
find_occur([PslA | PsgA], [PslB | PsgB]) when length(hd(PslA)) < length(hd(PslB)) ->
	{Match, Overflow} = lists:unzip([lists:split(length(hd(PslA)), Ps) || Ps <- PslB]),
	find_occur([PslA | PsgA], gappend(gappend(PsgB, Match), Overflow)).


%%	获取牌分数
-spec get_profit(Card) -> byte() when
      Card :: card().

get_profit(#sjc{point=P}) when P == 5; P == 10 ->
	P;
get_profit(#sjc{point=13}) ->
	10;
get_profit([Card | Cards]) ->
	get_profit(Card) + get_profit(Cards);	
get_profit(_) ->
	0.


%%	获取牌型
-spec get_type(Deal) -> byte() when
      Deal :: deal().

get_type([_Card]) ->
	?CONST_SJ_XW_SIGLE;
get_type([Card, Card]) ->
	?CONST_SJ_XW_COUPLE;
get_type(Cards) ->
	DeduplePoints = [Point || #sjc{ref_point=Point} <- lists:usort(Cards)],
	CpsCheckTractor = fun
						 (LastPoint, [Point | Points], Cps) when LastPoint + ?one == Point -> Cps(Point, Points, Cps);
						 (_LastPoint, [], _Cps) when length(DeduplePoints) * ?two == length(Cards) -> ?CONST_SJ_XW_TRACTOR;
						 (_LastPoint, _Points, _Cps) -> ?CONST_SJ_XW_THROW
					  end,
	CpsCheckTractor(hd(DeduplePoints), tl(DeduplePoints), CpsCheckTractor).


%%	获取拖拉机次数
-spec get_tractor_times(Ts, Tp, Deals, Deal) -> byte() when
      Ts :: byte(),
      Tp :: byte(),
      Deals :: deals(),
      Deal :: deal().
		  
get_tractor_times(Ts, Tp, [], Cards) ->
	length(lists:merge([Psl || Psl <- group(get_point(Ts, Tp, Cards)), length(hd(Psl)) > ?two]));
get_tractor_times(Ts, Tp, [Rcards|_], Cards) ->
	Rtsg = [Psl || Psl <- group(get_point(Ts, Tp, Cards)), length(hd(Psl)) > ?two],
	Tsg = [Psl || Psl <- group(get_point(Ts, Tp, Rcards)), length(hd(Psl)) > ?two],
	case length(lists:merge(Rtsg)) of
		?zero	->
			?zero;
		Rlength	->
			Length = length(lists:merge(Tsg)),
			?IF(Length > Rlength, Rlength, Length)
	end.


%%	通过花色点数获取相对点数
-spec get_point(Ts, Tp, StuffA) -> StuffB when
      Ts :: byte(),
      Tp :: byte(),
      StuffA :: Stuff,
      StuffB :: Stuff,
      Stuff :: card() | deal().

get_point(Ts, Tp, Cards) when is_list(Cards) ->
	[get_point(Ts, Tp, Card) || Card <- Cards];
get_point(Ts, _Tp, Card=#sjc{suit=?trump,point=P}) ->
	Rp = ?extend_point + P + ?IF(Ts == ?trump, -?one, ?zero),
	Card#sjc{ref_point=Rp};
get_point(Ts, Tp, Card=#sjc{suit=S,point=Tp}) ->
	Rp = ?extend_point + ?ace + ?IF(Ts == S, ?one, ?zero),
	Card#sjc{ref_point=Rp};
get_point(Ts, Tp, Card=#sjc{suit=S,point=P}) ->
	Rp = ?IF(Ts == S, ?extend_point, ?zero) + ?IF(P > Tp, P - 1, P),
	Card#sjc{ref_point=Rp}.


%%	判断出牌动作
-spec lead_action(Ts, Tp, Deal, RoundDeal, MaxDeal, IsBiggest) -> byte() when
      Ts :: byte(),
      Tp :: byte(),
      Deal :: deal(),
      RoundDeal :: deal(),
      MaxDeal :: deal(),
      IsBiggest :: boolean().

lead_action(Ts, Tp, Cards, Rcards, Mcards, IsBiggest) ->
	[#sjc{suit=Suit,point=Point} | _] = Rcards,
	Rs = ?IF(Point == Tp orelse Point > ?ace, Ts, Suit),
	case Rs == Ts orelse eq_suit(Ts, Tp, Rs, Cards) orelse not eq_suit(Ts, Tp, Ts, Cards) of
		?true	-> 
			?justlead;
		?false	->
			case eq_suit(Ts, Tp, Ts, Mcards) of
				?true	->
					?IF(IsBiggest, ?defeatist, ?justlead);
				?false	->
					?defeat
			end
	end.			
	

%%	相同牌型中牌点最大的一组牌
-spec max_cards(Psl) -> Deal when
      Psl :: deals(),
      Deal :: deal().
		  
max_cards(Psl) ->
	util:lists_best(fun(X, Y) -> (hd(X))#sjc.ref_point > (hd(Y))#sjc.ref_point end, Psl).


%%	相同牌型中牌点最小的一组牌
-spec min_cards(Psl) -> Deal when
      Psl :: deals(),
      Deal :: deal().

min_cards(Psl) ->
	util:lists_best(fun(X, Y) -> (hd(X))#sjc.ref_point < (hd(Y))#sjc.ref_point end, Psl).


%%	出牌
-spec lead(Deal, MaxDeal, RoundDeal) -> SmallerDeal when
      Deal :: deal(),
      MaxDeal :: deal(),
      RoundDeal :: deal(),
      SmallerDeal :: deal().

lead(Points, Mps, Rps) ->
	RstructLen = length(hd(hd(group(Rps)))),
	Mpbs = [lists:sublist(max_cards(Psl), RstructLen) || Psl <- group(Mps), length(hd(Psl)) >= RstructLen],
	try_lead([max_cards(Mpbs)], [Points]).


%%	甩牌
-spec throw(ThrowDeal, HandDeals) -> IsCanThrow when
      ThrowDeal :: deal(),
      HandDeals :: deals(),
      IsCanThrow :: ?true | SmallerDeal,
      SmallerDeal :: deal().

throw(Points, PsList) ->
	Pbests = [min_cards(Psl) || Psl <- group(Points)],
	SortFun = fun([C1|_], [C2|_]) -> C1#sjc.ref_point < C2#sjc.ref_point end,
	try_lead(lists:sort(SortFun, Pbests), PsList).


%%	尝试出牌
-spec try_lead(Deal, CompareDeals) -> LeadResult when
      Deal :: deal(),
      CompareDeals :: deals(),
      LeadResult :: ?true | SmallerDeal,
      SmallerDeal :: deal().

try_lead(Pbests, [Points | PointsList]) ->
	Opbests = [max_cards(Psl) || Psl <- group(Points)],
	case util:lists_all(fun(Pbest) -> compare(Pbest, Opbests) end, Pbests) of
		?true		-> try_lead(Pbests, PointsList);
		SmallerDeal	-> SmallerDeal
	end;
try_lead(_PbPairs, []) ->
	?true.


%%	比牌
-spec compare(DealA, DealB) -> GT | LT when
      DealA :: deal(),
      DealB :: deal(),
      GT :: ?true,
      LT :: DealA.

compare(Points, Pbests) ->
	{P, Len} = {hd(Points), length(Points)},
	Ps = [(hd(Ps))#sjc.ref_point || Ps <- Pbests, length(Ps) >= Len],
	?IF(P#sjc.ref_point >= util:lists_max(Ps, ?zero), ?true, Points).


%%	相同牌分组
-spec group(Deal) -> Group when
      Deal :: deal(),
      Group :: [deals()].
		  
group(List) ->
	Slist = lists:keysort(#sjc.ref_point, List),
	List1 = grouping(group_by(fun(A, B) -> A == B end, Slist)),
	List2 = lists:sort(fun(A, B) -> length(A) > length(B) end, List1),
	group_by(fun(A, B) -> length(A) == length(B) end, List2).


%%	相同元素分组
-spec group_by(Pred, List) -> [[T]] when
      Pred :: fun((Elem :: T) -> boolean()),
      List :: [T],
      T :: term().

group_by(Pred, [X | Xs]) ->
	{Ys, Zs} = lists:splitwith(fun(E) -> Pred(X, E) end, Xs),
	[[X | Ys] | group_by(Pred, Zs)];
group_by(_Fun, []) ->
	[].

%%	截取拖拉机
-spec grouping(ListsA) -> ListsB when
      ListsA :: [List],
      ListsB :: [List],
      List :: [term()].

grouping([[A, A], [B, B] | PsList]) when A#sjc.ref_point + ?one == B#sjc.ref_point ->
	[Ps | PsListNew] = grouping([[B, B] | PsList]),
	[[A, A | Ps] | PsListNew];
grouping([Ps | PsList]) ->
	[Ps | grouping(PsList)];
grouping([]) ->
	[].


%%	group append
-spec gappend(GroupA, Lists) -> GroupB when
      GroupA :: [[List]],
      GroupB :: [[List]],
      Lists :: [List],
      List :: [term()].
		  
gappend([PslA | Group], PslB) when length(hd(PslA)) == length(hd(PslB)) ->
	[PslA ++ PslB | Group];
gappend([PslA | Group], PslB) when length(hd(PslA)) < length(hd(PslB)) ->
	[PslB, PslA | Group];
gappend([PslA | Group], PslB) when length(hd(PslA)) > length(hd(PslB)) ->
	[PslA | gappend(Group, PslB)];
gappend([], PslB) ->
	[PslB].

%%	group delete
-spec gdelete(GroupA, Lists) -> GroupB when
      GroupA :: [[List]],
      GroupB :: [[List]],
      Lists :: [List],
      List :: [term()].

gdelete(Group, Psl) ->
	[].


%%	计算抄底分数
-spec eval_flip(StandFor, Deal, DealHeap) -> Score when
      StandFor :: byte(),
      Deal :: deal(),
      DealHeap :: deals(),
      Score :: integer().

eval_flip(?defender, _Ps, _Heap) ->
	0;
eval_flip(?attacker, Ps, Heap) ->
	MaxLen = length(hd(hd(group(Ps)))),
	get_profit(Heap) bsl MaxLen.


%%	计算倍率
-spec eval_rate(Score) -> Rate when
      Score :: integer(),
      Rate :: byte().
	
eval_rate(0) ->
	5;
eval_rate(Score) ->
	case (Score / 40) - 1 of
		Val when Val >= 1 ->
			(Score div 40) - 1;
		Val	when Val < 0 ->
			eval_rate(Score + 40) + 2;
		_Val ->
			(Score div 40)
	end.


%%	计算等级
-spec eval_level(Score) -> Level when
      Score :: integer(),
      Level :: byte().

eval_level(0) ->
	3;
eval_level(Score) ->
	abs(Score div 40 - 2).
