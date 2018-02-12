-module(function).
-export([foreachc/2, mapc/2, asc/2, desc/2]).
-export([filterc/2, concat_list/2, qsort/2]).

foreachc([], _) ->
    ok;
foreachc([X | XS], F) ->
    F(X),
    foreachc(XS, F).

mapc(_, []) ->
    [];
mapc(F, [X | XS]) ->
    [F(X) | mapc(F, XS)].

asc(X, Y) ->
    X > Y.

desc(X, Y) ->
    X < Y.

filterc(F, [X | XS]) ->
    B = F(X),
    if B -> [X | filterc(F, XS)]; true -> filterc(F, XS) end;
filterc(_, []) ->
    [].

concat_list(Tar, []) ->
    Tar;
concat_list([], Tar) ->
    Tar;
concat_list([X | XS], Tar) ->
    [X | concat_list(XS, Tar)].

qsort(F, [X | XS]) ->
    ShortL = qsort(F, filterc(fun(Y) -> F(X, Y) end, XS)),
    LonngL = qsort(F, filterc(fun(Y) -> not F(X, Y) end, XS)),
    concat_list(ShortL, [X | LonngL]);
qsort(_, []) -> 
    [].

