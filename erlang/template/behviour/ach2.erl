%%%-------------------------------------------------------------------
%%% @author ldcc
%%% @copyright (C) 2018, <ldcc>
%%% Created : 28. 二月 2018 9:52
%%%-------------------------------------------------------------------
-module(ach2).
-author("ldcc").

-behaviour(interface).

%% API
-export([foo/1, bar/1, baz/1, qux/1, quux/1]).


foo(Foo) ->
    {ok, Foo}.

bar(Bar) ->
    {ok, Bar}.

baz(Baz) ->
    {ok, Baz}.

qux(Qux) ->
    {ok, Qux}.

quux(Quux) ->
    {ok, Quux}.