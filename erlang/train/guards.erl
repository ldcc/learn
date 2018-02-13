-module(guards).
-export([maximum/1, maximum_tr/1, reverse_tr/1]).
-export([temperature/1, namec/1, tempc/1, unitc/1]).
-export([test_if/2, test_case/1]).

%% dont handled error so soon

%% maximum
% maximum([]) -> error("empty list");
maximum([E1]) -> 
    E1;
maximum([E1 | E2]) -> 
    max(E1, maximum(E2)).

% maximum_tr([]) -> error("empty list");
maximum_tr([E1 | E2]) -> 
    maximum_tr(E1, E2).

maximum_tr(Max, []) -> 
    Max;
maximum_tr(Max, [E1 | E2]) -> 
    maximum_tr(max(Max, E1), E2).

%% reverse
reverse_tr(L) -> 
    reverse_tr_iter(L, []).

reverse_tr_iter([], Tar) -> 
    Tar;
reverse_tr_iter([E1 | E2], Tar) -> 
    reverse_tr_iter(E2, [E1 | Tar]).

%% temperature
temperature(L) ->
    RL = convert_temp(L),
    Pole = find_pole(RL),
    print_temp(RL),
    print_pole(Pole).

convert_temp([{N, {f, T}} | C2]) ->
    C1 = {N, {c, 5 / 9 * (T - 32)}},
    [C1 | convert_temp(C2)];
convert_temp([C1 | C2]) -> 
    [C1 | convert_temp(C2)];
convert_temp([]) -> 
    [].

find_pole([C1 | C2]) -> 
    find_pole({C1, C1}, C2).

find_pole({Max_C, Min_C}, [C1 | C2]) -> 
    Max = max_temp(Max_C, C1),
    Min = min_temp(Min_C, C1),
    find_pole({Max, Min}, C2);

find_pole(Pole, []) ->
    Pole.

%% too longer
% max_temp({N1, {U1, T1}}, {N2, {U2, T2}}) ->
%     if T1 > T2 -> {N1, {U1, T1}}; true -> {N2, {U2, T2}} end.

% min_temp({N1, {U1, T1}}, {N2, {U2, T2}}) ->
%     if T1 < T2 -> {N1, {U1, T1}}; true -> {N2, {U2, T2}} end.

%% no way
% max_temp(C1, C2) ->
%     if tempc(C1) > tempc(C2) -> C1; true -> C2 end.

% min_temp(C1, C2) ->
%     if tempc(C1) < tempc(C2) -> C1; true -> C2 end.

max_temp(C1, C2) ->
    B = tempc(C1) > tempc(C2),
    if B -> C1; true -> C2 end.

min_temp(C1, C2) ->
    B = tempc(C1) < tempc(C2),
    if B -> C1; true -> C2 end.

namec({C, {_, _}}) ->
    C.

unitc({_, {U, _}}) ->
    U.

tempc({_, {_, T}}) ->
    T.

print_temp([{N, {_, T}} | C2]) ->
    io:format("~-15w ~w c~n", [N, T]),
    print_temp(C2);
print_temp([]) -> ok.

print_pole({Max, Min}) ->
    io:format("Max temperature was ~w c in ~w~n", [tempc(Max), namec(Max)]),
    io:format("Min temperature was ~w c in ~w~n", [tempc(Min), namec(Min)]).


%% if & case
test_if(A, B) ->
    if
        A == 5 ->
            a_equals_5;
        B == 6 ->
            b_equals_6;
        %% A equals 2 and B equals 3
        A == 2, B == 3 -> 
        % A == 2 andalso B == 3 -> 
            a_equals_2_b_equals_3;
        %% A equals 1 or B equals 7
        % A == 1 orelse B == 7 -> 
        A == 1; B == 7 ->
            a_equals_1_or_b_equals_7
    end.

test_case(Month) ->
    case Month of
        jan -> 01;
        feb -> 02;
        mar -> 03;
        apr -> 04;
        may -> 05;
        jun -> 06;
        jul -> 07;
        aug -> 08;
        sep -> 09;
        oct -> 10;
        nov -> 11;
        dec -> 12
    end.