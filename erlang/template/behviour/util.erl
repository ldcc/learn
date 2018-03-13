%%%-------------------------------------------------------------------
%%% @author ldcc
%%% @copyright (C) 2018, <ldcc>
%%% Created : 28. 二月 2018 11:10
%%%-------------------------------------------------------------------
-module(util).
-author("ldcc").

-define(IF(Test, Then, Else), case Test of true -> Then; false -> Else end).

%% API
-export([
    lists_max/2,
    lists_map/2,
    lists_all/2,
    lists_any/2,
    lists_best/2,

    round/2,
    handle_delay_action/1
]).

%% lists_map(fun F/N, [[A1...], ..., [N1...]]) 
%% => [F(A1, ..., N1), ..., F(An, ..., Nn...)]
lists_map(Fun, ArgsList) ->
    case lists:member([], ArgsList) of
        true ->
            [];
        false ->
            Val = apply(Fun, [hd(Args) || Args <- ArgsList]),
            [Val | lists_map(Fun, [tl(Args) || Args <- ArgsList])]
    end.

lists_any(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        false ->
            lists_any(Pred, Tail);
        Target ->
            Target
    end;
lists_any(_Pred, []) ->
    false.

lists_all(Pred, [Hd | Tail]) ->
    case Pred(Hd) of
        true ->
            lists_all(Pred, Tail);
        Satisless ->
            Satisless
    end;
lists_all(_Pred, []) ->
    true.

lists_best(_Compare, [Val]) ->
    Val;
lists_best(Compare, [Val | List]) ->
    Most = lists_best(Compare, List),
    ?IF(Compare(Val, Most), Val, Most);
lists_best(_Compare, []) ->
    [].

round(Number, N) when N < 10 ->
    round(Number);
round(Number, N) ->
    10 * round(Number / 10, N / 10).

handle_delay_action([{Fun, Args, 0} | Actions]) ->
    begin apply(Fun, Args), handle_delay_action(Actions) end;
handle_delay_action([{Fun, Args, Sec} | Actions]) ->
    [{Fun, Args, Sec - 1} | handle_delay_action(Actions)];
handle_delay_action([]) ->
    [].

lists_max([], Def) ->
    Def;
lists_max(List, _Def) ->
    lists:max(List).
