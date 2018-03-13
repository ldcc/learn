%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : ai_zpsybp_api.erl
%%% Created : 
%%% -------------------------------------------------------------------
-module(ai_api).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.desk_scene.hrl").
-include("mod.game_zpsybp.hrl").  

-compile(export_all).

%% 扩展导出
-export([]).


%% ====================================================================
%% Internal functions
%% ====================================================================

%%	牌信息判断
is_red(#bpc{point=P}) -> 
	P == ?two orelse P == ?seven orelse P == ?ten.

is_upper(#bpc{kind=?two}) -> 
	?true;
is_upper(?two) -> 
	?true;
is_upper(_Kind) -> 
	?false.



%%	一副牌
get_pai_all() -> 
	PL = get_pai_case(),
	lists:flatten(lists:duplicate(?four, PL)).


%%	所有牌型
get_pai_case() ->
	[#bpc{kind=K,point=P} || K <- ?CARD_TYPE, P <- ?CARD_POINT].


%%	发牌
fapai(?PLAYER_NUM1) ->
	fapai(?PLAYER_NUM1, ?CARD_NUM1);
fapai(?PLAYER_NUM2) ->
	fapai(?PLAYER_NUM2, ?CARD_NUM2);
fapai(PlayerNum) ->
	fapai(PlayerNum, ?CARD_NUM1).

fapai(PlayerNum, CardNum) ->
	CardHeap = util:lists_shuffle(get_pai_all()),
	fapai(PlayerNum, CardNum, {[], CardHeap}).

fapai(?one, CardNum, {OrderAcc, Stack}) ->
	{Order, StackNew} = fapai_rnd(CardNum + 1, Stack),
	{[Order|OrderAcc], StackNew};
fapai(PlayerNum, CardNum, {OrderAcc, Stack}) ->
	{Order, StackNew} = fapai_rnd(CardNum, Stack),
	AccsNew = {[Order|OrderAcc], StackNew},
	fapai(PlayerNum - 1, CardNum, AccsNew).



%%	发N张牌
fapai_rnd(CardNum, Stack) ->
	{Order, StackNew} = fapai_rnd(CardNum, ?zero, {[], Stack}),
	{sortout_deal(Order), StackNew}.

fapai_rnd(?zero, _TiTimes, {Order, Stack}) ->
	{Order, Stack};
fapai_rnd(N, TiTimes, {Order, [Card | Stack]}) ->
	NewOrder = append_by(fun lists:member/2, Card, Order),
	TiTimesCurr = length(lists:filter(fun(Deal) -> length(Deal) == ?four end, NewOrder)),
	NewN = ?IF(TiTimesCurr > ?one andalso TiTimesCurr > TiTimes, N, N - ?one),
	fapai_rnd(NewN, TiTimesCurr, {NewOrder, Stack}).



%%	整理手牌
%%	Deals => [[Card...]...]
%% sortout_deal(Deals) ->
%% 	{Tis, Multip} = lists:partition(fun(Deal) -> length(Deal) == ?four end, Deals),
%% 	{Kans, Except} = lists:partition(fun(Deal) -> length(Deal) == ?three end, Multip),
%% 	TisNew = alter_cart_type_to(?CONST_ZPSYBP_CARD_STATE_TI, Tis),
%% 	KansNew = alter_cart_type_to(?CONST_ZPSYBP_CARD_STATE_KAN, Kans),
%% 	Compare = fun(D1, D2) -> 
%% 					  begin P1 = profit_curr_get(D1), P2 = profit_curr_get(D2) end,
%% 					  P1 > P2 orelse P1 == P2 andalso length(D1) > length(D2)			  
%% 			  end,
%% 	[Cards | Tails] = util:lists_best(Compare, flatten_tree(chi_possible(lists:sort(lists:flatten(Except)), []))),
%% 	Fun = fun(C, Deal) -> length(Deal) < ?ROW andalso lists:keymember(C#bpc.point, #bpc.point, Deal) end,
%% 	{Singles, BestDeal} = lists:partition(fun(Deal) -> length(Deal) == ?one end, append_by(Fun, Cards, Tails)),
%% 	SortFun = fun ([#bpc{kind=K1,point=P1}|_], [#bpc{kind=K2,point=P2}|_]) -> P1 < P2 orelse P1 == P2 andalso K1 < K2 end,
%% 	{TisNew, KansNew ++ lists:sort(SortFun, BestDeal) ++ lists:reverse(util:lists_split(lists:keysort(#bpc.point, lists:flatten(Singles)), ?ROW))}.

	
sortout_deal(Deals) ->
	{Single, Multip} = lists:partition(fun(Deal) -> length(Deal) == ?one end, Deals),
	{Double, Events} = lists:partition(fun(Deal) -> length(Deal) == ?two end, Multip),
	{Triple, Quadru} = lists:partition(fun(Deal) -> length(Deal) == ?three end, Events),
	TripleNew = alter_cart_type_to(?CONST_ZPSYBP_CARD_STATE_KAN, Triple),
	QuadruNew = alter_cart_type_to(?CONST_ZPSYBP_CARD_STATE_TI, Quadru),
	SingleFun = fun(#bpc{point=P}, Deal) -> lists:keymember(P, #bpc.point, Deal) end,
	DoubleNew = append_by(SingleFun, sort_with(Single, #bpc.kind), Double),
	{SingleNew, Multiple} = lists:partition(fun(Deal) -> length(Deal) == ?one end, DoubleNew),
	{QuadruNew, TripleNew ++ sortout_deal(SingleNew, Multiple, ?COLUMN - length(TripleNew))}.

sortout_deal([[Card] | Single], Multiple, Column) ->
	SortFun = fun(_, Deal) -> length(Deal) < ?ROW andalso length(Multiple) >= Column end,
	sortout_deal(Single, append_by(SortFun, Card, Multiple), Column);
sortout_deal([], Multiple, _Column) ->
	SortFun = fun(Deal) -> fun(Card1, Card2) -> occur(Card1, Deal) < occur(Card2, Deal) end end,
	[lists:sort(SortFun(Deal), Deal) || Deal <- sort_with(Multiple, #bpc.point, fun get_daeh/1)].



%%	检查偎提/偎跑
check_event(State, _HandDeal, OpenDeal, FocusCard, _BanDeal) when State == ?CONST_ZPSYBP_CARD_STATE_WTI; State == ?CONST_ZPSYBP_CARD_STATE_WPAO ->
	WeiCard = FocusCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_WEI},
	Allow = lists:member(lists:duplicate(?three, WeiCard), OpenDeal),
	?IF(Allow, lists:duplicate(?four, FocusCard#bpc{state=State}), []);
%%	检查提牌/跑牌
check_event(State, HandDeal, _OpenDeal, FocusCard, _BanDeal) when State == ?CONST_ZPSYBP_CARD_STATE_TI; State == ?CONST_ZPSYBP_CARD_STATE_PAO ->
	KanCard = FocusCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_KAN},
	Allow = occur(KanCard, HandDeal) == ?three,
	?IF(Allow, lists:duplicate(?four, FocusCard#bpc{state=State}), []);
%%	检查臭偎
check_event(?CONST_ZPSYBP_CARD_STATE_WEI, HandDeal, _OpenDeal, DrawCard, _BanDeal) ->
	HandDealTmp = [DrawCard| [Card || #bpc{state=S}=Card <- HandDeal, S =/= ?CONST_ZPSYBP_CARD_STATE_KAN]],
	Allow = occur(DrawCard, HandDeal) == ?two andalso length(HandDealTmp) > ?three,
	?IF(Allow, lists:duplicate(?three, DrawCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_WEI}), []);
%%	检查偎牌/碰牌
check_event(?CONST_ZPSYBP_CARD_STATE_PONG, HandDeal, _OpenDeal, DisCard, BanDeal) ->
	ChouCard = DisCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_PONG},
	HandDealTmp = [DisCard| [Card || #bpc{state=S}=Card <- HandDeal, S =/= ?CONST_ZPSYBP_CARD_STATE_KAN]],
	Allow = occur(DisCard, HandDeal) == ?two andalso not lists:member(ChouCard, BanDeal) andalso length(HandDealTmp) > ?three,
	?IF(Allow, lists:duplicate(?three, DisCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_PONG}), []);
%%	检查吃牌
check_event(?CONST_ZPSYBP_CARD_STATE_CHI, HandDeal, _OpenDeal, Discard, BanDeal) ->
	NewHandDeal = [Discard | [Card || #bpc{state=S}=Card <- HandDeal, S =/= ?CONST_ZPSYBP_CARD_STATE_KAN]],
	Allow = not lists:member(Discard#bpc{state=?CONST_ZPSYBP_CARD_STATE_CHI}, BanDeal),
	?IF(Allow, check_chi(NewHandDeal, Discard), []); 
%%	检查碰跑
check_event(?CONST_ZPSYBP_CARD_STATE_PPAO, _HandDeal, OpenDeal, FocusCard, _BanDeal) ->
	PongDeal = lists:duplicate(?three, FocusCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_PONG}),
	Allow = lists:member(PongDeal, OpenDeal),
	?IF(Allow, lists:duplicate(?four, FocusCard#bpc{state=?CONST_ZPSYBP_CARD_STATE_PPAO}), []);
%%	检查胡牌
check_event(?CONST_ZPSYBP_CARD_STATE_WIN, HandDeal, OpenDeal, HuCard, BanDeal) ->
	begin KanDeal = get_all_kan(HandDeal), Cards1 = deep_rember(KanDeal, HandDeal), Cards2 = [HuCard | Cards1] end,
	CheckH = fun(IdleCards) -> check_hu(fun any/2, IdleCards, KanDeal ++ OpenDeal) end,
	CheckE = fun(Events) -> lists:any(fun (Event) -> check_event(Event, HandDeal, OpenDeal, HuCard, BanDeal) =/= [] end, Events) end,
	?IF(empty(?IF(CheckE(?PAOS), ?IF(empty(CheckH(Cards1)), CheckH(Cards2), [HuCard]), CheckH(Cards2))), [], [HuCard]);
check_event(_EventType, _HandDeal, _OpenDeal, _DrawCard, _BanDeal) ->
	[].


%%	检查是吃牌还是摆火
check_chi(HandDeal, _Discard) when length(HandDeal) < ?four ->
	?null;
check_chi(HandDeal, Discard) ->
	Deals = [lists:sort(fun(Card, _) -> Card /= Discard end, Deal) || Deal <- get_have_chi(HandDeal, Discard)],
	case occur(Discard, HandDeal) of
		?zero	-> [];
		?one	-> ?IF(empty(Deals), ?null, [{Deal, []} || Deal <- Deals]);
		_Else	->
			Fun = fun(Deal) -> {Deal, check_chi(HandDeal -- Deal, Discard)} end,
			IDeals = [{Deal, Deals} || {Deal, Deals} <- [Fun(Deal) || Deal <- Deals], Deals =/= ?null],
			?IF(empty(IDeals), ?null, IDeals)
	end.


%%	dfs穷举吃牌
chi_possible(Cards, [Card | Tail]) ->
	[chi_possible([Card | Cards] ++ Tail, [], []) | chi_possible([Card | Cards], Tail)];
chi_possible(Cards, []) ->
	chi_possible(Cards, [], []).

chi_possible([], BestDeal, Singles) ->
	[Singles | BestDeal];
chi_possible([Card | Tails] = Cards, Deals, Singles) ->
	case get_have_chi(Cards, Card) of
		[]		->
			chi_possible(Tails, Deals, [Card | Singles]);
		TryDeal	->
			filtermap(fun(Deal) -> chi_possible(Cards -- Deal, [Deal | Deals], Singles) end, TryDeal)
	end.


%%	dfs检查胡牌
check_hu(Check, Cards, EventDeal) ->
	HuDeals = hu_possible(Check, Cards, [], EventDeal),
	maximum(fun profit_curr_get/1, flatten_tree(HuDeals)).

hu_possible(_Try, [], HuDeal, EventDeal) ->
	?IF(profit_satisfy_check(HuDeal ++ EventDeal), lists:sort(HuDeal) ++ EventDeal, []);
hu_possible(_Try, [Card, Card] = Deal, HuDeal, EventDeal) ->
	?IF(profit_satisfy_check(HuDeal ++ EventDeal), lists:sort([Deal | HuDeal]) ++ EventDeal, []);
hu_possible(Try, Cards, HuDeal, EventDeal) ->
	TryCards = lists:sublist(lists:usort(Cards), ?two),
	Try(fun(Card) -> hu_possible(Try, Cards, HuDeal, EventDeal, Card) end, TryCards).

hu_possible(Try, Cards, HuDeal, EventDeal, Card) ->
	TryDeal = get_have_kan(Cards, Card) ++ get_have_chi(Cards, Card),
	Fun = fun(Deal) -> hu_possible(Try, Cards -- Deal, [Deal | HuDeal], EventDeal) end,
	?IF(empty(TryDeal), [], Try(Fun, TryDeal)).


%%	手牌中所有对子 
get_all_pair([Card, Card | Deal]) ->
	[[Card, Card] | get_all_pair(Deal)];
get_all_pair([_Card | Deal]) ->
	get_all_pair(Deal);
get_all_pair([]) ->
	[].


%%	手牌中所有坎牌 
get_all_kan([[Card, Card, Card] | DealTail]) ->
	[[Card, Card, Card] | get_all_kan(DealTail)];
get_all_kan([Card, Card, Card | Deal]) ->
	[[Card, Card, Card] | get_all_kan(Deal)];
get_all_kan([_Card | Deal]) ->
	get_all_kan(Deal);
get_all_kan([]) ->
	[].


%%	检查该牌是否拥有3张 
get_have_kan(Cards, CheckCard) ->
	Kan = lists:filter(fun(Card) -> equal(CheckCard, Card) end, Cards),
	case length(Kan) == ?three of
		?true	-> [Kan];
		?false	-> []
	end.


%%	手牌中能吃该牌的所有组合
get_have_chi(HandDeal, Discard) ->
	deduplicate(get_have_sentence(HandDeal, Discard) ++ get_have_twist(HandDeal, Discard)).


%%	手牌中能与该牌组成一句话的组合
get_have_sentence(HandDeal, #bpc{kind=Kind,point=Point}=Card) ->
	DedupSortedDeal = lists:usort([Card || #bpc{kind=K}=Card <- HandDeal, K == Kind]),
	DealList = [Card || #bpc{point=P} = Card <- DedupSortedDeal, abs(P - Point) =< ?two],
	case is_red(Card) of
		?true	->
			RedDeal = [Card || Card <- DedupSortedDeal, is_red(Card)],
			get_have_sentence(DealList) ++ get_have_sentence(RedDeal);
		?false	->
			get_have_sentence(DealList)
	end.

get_have_sentence([_ | Tail] = DealList) when length(DealList) >= ?three ->
	Sentence = lists:sublist(DealList, ?three),
	TailSentense = get_have_sentence(Tail),
	?IF(check_is_sentence(Sentence), [Sentence | TailSentense], TailSentense);
get_have_sentence(_) ->
	[].


%%	检查是否为一句话
check_is_sentence([#bpc{kind=K,point=P1}, #bpc{kind=K,point=P2}, #bpc{kind=K,point=P3}]) ->
	P1 + ?one == P2 andalso P2 + ?one == P3 orelse P1 == ?two andalso P2 == ?seven andalso P3 == ?ten;
check_is_sentence(_) ->
	?false.


%%	检查吃是否有胡息 (一句话有胡息,绞牌无胡息)
check_chi_profit([#bpc{kind=K,point=P1}, #bpc{kind=K,point=P2}, #bpc{kind=K,point=P3}]) ->
	P1 == ?one andalso P2 == ?two andalso P3 == ?three orelse P1 == ?two andalso P2 == ?seven andalso P3 == ?ten;
check_chi_profit(_Deal) ->
	?false.


%%	手牌中能与该牌组成绞牌的组合
get_have_twist(HandDeal, #bpc{point=Point}) ->
	Deal = [Card || #bpc{point=P}=Card <- HandDeal, P =:= Point],
	{UpperDeal, LowerDeal} = lists:partition(fun is_upper/1, Deal),
	{Deal1, Deal2} = {lists:sublist(UpperDeal, 2), lists:sublist(LowerDeal, 2)},
	Fun = fun(Card) -> [Card | ?IF(is_upper(Card), Deal2, Deal1)] end,
	TwistListSet = [Fun(Card) || Card <- deduplicate(Deal)],
	get_have_twist(TwistListSet).

get_have_twist([Twist | Tail]) ->
	TailTwist = get_have_twist(Tail),
	?IF(check_is_twist(Twist), [Twist | TailTwist], TailTwist);
get_have_twist([]) ->
	[].


%%	检查是否为绞牌
check_is_twist([#bpc{kind=S1,point=P}, #bpc{kind=S,point=P}, #bpc{kind=S2,point=P}]) ->
	(S =:= S1 orelse S =:= S2) andalso S1 =/= S2;
check_is_twist(_) ->
	?false.


%%	检查是否满足胡牌所需的胡息
profit_satisfy_check(Deals) ->
	profit_curr_get(Deals) >= ?CONST_ZPSYBP_WIN_REQUIRE_PROFIT.


%%	计算已知胡息
profit_curr_get(Deals) ->
	lists:sum([profit_get(Deal) || Deal <- Deals]).


%%	寻找潜在胡息
profit_hide_find(Deals) ->
	Kans = get_all_kan(Deals),
	ProfitPoints = [?one, ?two, ?three, ?seven, ?ten],
	IdleCards = [Card || #bpc{point=Point}=Card <- lists:flatten([Deals -- Kans]), lists:member(Point, ProfitPoints)],
	{Upper, Lower} = lists:partition(fun is_upper/1, IdleCards),
	profit_curr_get(Kans) + profit_chi_find(Upper, ?two) + profit_chi_find(Lower, ?one).

profit_chi_find(Deal, Kind) -> 
	Profit = ?IF(is_upper(Kind), ?six, ?three),
	case all_member(?make123(Kind), Deal) of
		?true	-> Profit + profit_chi_find(Deal -- ?make123(Kind), Kind);
		?false	->
			case all_member(?make274(Kind), Deal) of
				?true	-> Profit + profit_chi_find(Deal -- ?make274(Kind), Kind);
				?false	-> ?zero
			end
	end.


%%	根据牌型获取胡息
profit_get(Deal) ->
	#bpc{state=State} = hd(Deal),
	profit_get(State, sort_with(Deal,#bpc.point)).

profit_get(State, [Card, Card, Card]) ->
	LowerEvents = [?CONST_ZPSYBP_CARD_STATE_NORMAL, ?CONST_ZPSYBP_CARD_STATE_PONG, ?CONST_ZPSYBP_CARD_STATE_WIN],
	?IF(lists:member(State, LowerEvents), ?IF(is_upper(Card), ?three, ?one), ?IF(is_upper(Card), ?six, ?three));
profit_get(State, [Card, Card, Card, Card]) ->
	?IF(lists:member(State, ?PAOS), ?IF(is_upper(Card), ?nine, ?six), ?IF(is_upper(Card), ?twelve, ?nine));
profit_get(Type, Deal) ->
	case length(Deal) of
		Len when Len < ?three	->
			?zero;
		Len when Len > ?three	->
			{DealChi, DealTail} = lists:split(?three, Deal),
			profit_get(Type, DealChi) + profit_get(Type, DealTail);
		?three	-> 
			?IF(check_chi_profit(Deal), ?IF(is_upper(hd(Deal)), ?six, ?three), ?zero)
	end.


%%	处理事件
event_handler(HandDeal, OpenDeal, Card, ?CONST_ZPSYBP_CARD_STATE_WIN) ->
	KanDeal = get_all_kan(HandDeal),
	Deals = lists:flatten(deep_rember(KanDeal, HandDeal)),
	check_hu(fun filtermap/2, [Card | Deals], KanDeal ++ OpenDeal);
event_handler(HandDeal, OpenDeal, Deals, State) ->
	AlrTp = lists:member(State, ?TIS ++ ?PAOS) andalso lists:any(fun(Deal) -> length(Deal) == ?four end, OpenDeal),
	event_handler(HandDeal, OpenDeal, Deals, State, ?IF(AlrTp, ?DRAWING, ?DISCARDING)).

%%	偎提/偎跑/碰跑
event_handler(HandDeal, OpenDeal, Deals, State, DeskState) 
  when State == ?CONST_ZPSYBP_CARD_STATE_WTI; State == ?CONST_ZPSYBP_CARD_STATE_WPAO; State == ?CONST_ZPSYBP_CARD_STATE_PPAO ->
	OpenDealNew = deep_rember(Deals, OpenDeal),
	{HandDeal, Deals ++ OpenDealNew, DeskState};
%%	偎牌/臭偎/提牌/跑牌/碰牌/吃牌
event_handler(HandDeal, OpenDeal, Deals, _State, DeskState) ->
	HandDealNew = deep_rember(Deals, HandDeal),
	{HandDealNew, Deals ++ OpenDeal, DeskState}.


%%	------------------------------------------------------------------------------------------------------
%%	------------------------------------------------------------------------------------------------------
%%	------------------------------------------------------------------------------------------------------

%%	指定Key排序
sort_with(List, Key) ->
	Compare = fun(A, B) -> A < B end,
	sort_with(Compare, List, Key, fun get_base/1).
sort_with(List, Key, Exp) when is_number(Key) ->
	Compare = fun(A, B) -> A < B end,
	sort_with(Compare, List, Key, Exp);
sort_with(Compare, List, Exp) when is_list(List) ->
	Fun = fun (Val1, Val2) -> Compare(Exp(Val1), Exp(Val2)) end,
	lists:sort(Fun, List).

sort_with(Compare, List, Key, Exp) ->
	Fun = fun (Val1, Val2) -> Compare(element(Key, Exp(Val1)), element(Key, Exp(Val2))) end,
	lists:sort(Fun, List).


%%	按规则分组
append_by(_Fun, [], Lists) -> 
	[List || List <- Lists, List =/= []];
append_by(Fun, [Val | Vals], List) ->
	append_by(Fun, Vals, append_by(Fun, Val, List));
append_by(Fun, Val, [List | Tail]) ->
	?IF(Fun(Val, List), [[Val | List] | Tail], [List | append_by(Fun, Val, Tail)]);
append_by(_Fun, Val, []) -> 
	[[Val]].


%%	多维度 nth
nd_nth(Lists, [N | Ns]) ->
	nd_nth(lists:nth(N, Lists), Ns);
nd_nth(Val, []) ->
	Val.


%%	魔改的 equal 函数
equal(#bpc{kind=K,point=P}, #bpc{kind=K,point=P}) -> 
	?true;
equal(Val, Val) ->
	?true;
equal(_Val1, _Val2) ->
	?false.


%%	魔改的 empty 函数
empty([]) -> 
	?true;
empty(<<>>) -> 
	?true;
empty(?null) -> 
	?true;
empty(_Val) ->
	?false.


%%	魔改的 member 函数
member(Val1, [Val2 | List]) ->
	?IF(equal(Val1, Val2), ?true, member(Val1, List));
member(_Val, []) -> 
	?false.


%%	魔改的 any 函数
any(Fun, [Hd | Tail]) ->
	case Fun(Hd) of
		?false	-> any(Fun, Tail);
		[]		-> any(Fun, Tail);
		Val		-> Val
	end;
any(_Pred, []) -> 
	[]. 


%%	魔改的 filtermap 函数
filtermap(Fun, Olds) ->
	case filtermap(Fun, Olds, []) of
		[Val]	-> Val;
		News	-> News
	end.
	
filtermap(Fun, [Hd | Tail], List) ->
	case Fun(Hd) of
		?false	-> filtermap(Fun, Tail, List);
		[]		-> filtermap(Fun, Tail, List);
		Val		-> filtermap(Fun, Tail, ?IF(member(Val, List), List, [Val | List]))
	end;
filtermap(_Fun, [], List) -> 
	List.


%%	replce 函数
replace(_New, _Old, []) ->
	[];
replace(New, Old, [Old | Tail]) ->
	[New | Tail];
replace(New, Old, [Val | Tail]) ->
	[Val | replace(New, Old, Tail)].


%%	maximum 函数
maximum(_Fun, []) ->
	[];
maximum(_Fun, [Max]) ->
	Max;
maximum(Fun, [Hd | Tail]) ->
	Max = maximum(Fun, Tail),
	?IF(Fun(Hd) > Fun(Max), Hd, Max).


%%	minimum 函数
minimum(_Fun, []) ->
	[];
minimum(_Fun, [Min]) ->
	Min;
minimum(Fun, [Hd | Tail]) ->
	Min = minimum(Fun, Tail),
	?IF(Fun(Hd) < Fun(Min), Hd, Min).


%%	all_member 函数
all_member([Val|Vals], List) ->
	lists:member(Val, List) andalso all_member(Vals, List);
all_member([], _List) ->
	?true.	


%%	deep_rember 函数
deep_rember([List | Tail], Lists) when is_list(List) ->
	deep_rember(Tail, deep_rember(List, Lists));
deep_rember([Val | Tail], Lists) ->
	deep_rember(Tail, deep_delete(Val, Lists));
deep_rember([], Lists) ->
	[List || List <- Lists, List =/= []].


%%	deep_delete 函数
deep_delete(Val, [List | Tail]) when is_list(List) ->
	ListNew = deep_delete(Val, List),
	?IF(equal(List, ListNew), [List | deep_delete(Val, Tail)], [ListNew | Tail]);
deep_delete(Val1, [Val2 | Tail]) ->
	?IF(equal(Val1, Val2), Tail, [Val2 | deep_delete(Val1, Tail)]);
deep_delete(_Val, []) ->
	[].


%%	deep_sort 函数
deep_sort([Val | List]) ->
	lists:sort([deep_sort(Val) | deep_sort(List)]);
deep_sort(Val) ->
	Val.
	

%%	occcur 函数
occur(Val, [Val | Tail]) ->
	?one + occur(Val, Tail);
occur(Val, [_ | Tail]) ->
	occur(Val, Tail);
occur(_, []) ->
	?zero.


%%	deduplicate 函数
deduplicate(List) ->
	deduplicate(List, []).

deduplicate([Val | Tail], DedupleList) ->
	NewDedupleList = deduplicate(Tail, DedupleList),
	?IF(lists:member(Val, NewDedupleList), NewDedupleList, [Val | NewDedupleList]);
deduplicate([], DedupleList) ->
	DedupleList.


%%	将一组或多组牌转换为指定类型
alter_cart_type_to(Type, Deals) when Deals =/= [], is_list(hd(Deals)) ->
	[alter_cart_type_to(Type, Deal) || Deal <- Deals];
alter_cart_type_to(Type, Deal) ->
	[Card#bpc{state=Type} || Card <- Deal].


%%	获取嵌套列表第一个非列表元素
get_base(List) when is_list(List) ->
	get_base(hd(List));
get_base(Val) ->
	Val.


%%	获取嵌套列表最后一个非列表元素
get_daeh(List) when is_list(List) ->
	get_daeh(lists:last(List));
get_daeh(Val) ->
	Val.


%%	检查列表的嵌套层数
check_layer([Hd | _List]) ->
	?one + check_layer(Hd);
check_layer(_Item) ->
	?zero.


%%	消除列表中的多余嵌套
flatten_tree([]) ->
	[];
flatten_tree(Lists) ->
	case check_layer(Lists) > ?two of
		?true	-> flatten_tree(Lists, []);
		?false	-> [Lists]
	end.

flatten_tree([Old | Olds], News) ->
	flatten_tree(Old) ++ flatten_tree(Olds) ++ News;
flatten_tree([], Lists) ->
	Lists.


%%	一维列表根据坐标系转换多维列表
make_vector([], Vector) ->
	Vector;
make_vector([{PosData, Val} | Posits], Vector) ->
	make_vector(Posits, make_vector(PosData, Val, Vector)).

make_vector([?zero | Ps], Val, [H | T]) ->
	[make_vector(Ps, Val, H) | T];
make_vector([P | Ps], Val, [H | T]) ->
	[H | make_vector([P - 1 | Ps], Val, T)];
make_vector([_ | Ps], Val, []) ->
	[make_vector(Ps, Val, [])];
make_vector([], Val, []) ->
	Val.
